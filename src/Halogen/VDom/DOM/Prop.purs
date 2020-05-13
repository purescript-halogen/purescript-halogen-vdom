module Halogen.VDom.DOM.Prop
  ( Prop(..)
  , ElemRef(..)
  , PropValue
  , propFromString
  , propFromBoolean
  , propFromInt
  , propFromNumber
  , buildProp
  ) where

import Prelude

import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Nullable (null, toNullable, Nullable)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Uncurried as EFn
import Foreign (typeOf)
import Foreign.Object as Object
import Halogen.VDom as V
import Halogen.VDom.Machine (Step, Step'(..), mkStep)
import Halogen.VDom.Types (Namespace(..))
import Halogen.VDom.Util as Util
import Halogen.VDom.Util (STObject')
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element) as DOM
import Web.Event.Event (EventType(..), Event) as DOM
import Web.Event.EventTarget (eventListener, EventListener) as DOM

-- | Attributes, properties, event handlers, and element lifecycles.
-- | Parameterized by the type of handlers outputs.

-- | What is the difference between attributes and properties?
-- |
-- | Attributes are defined by HTML. Properties (on DOM elements) are defined by DOM.
-- | E.g. `class` attribute corresponds to `element.className` property
-- | almost always you should use properties on html elements, the svg elements don't have properties, only classes
-- | more https://github.com/purescript-halogen/purescript-halogen-vdom/issues/30#issuecomment-518015764
-- |
-- | Also, attributes can be only strings, props - strings, numbers, booleans
data Prop a
  = Attribute
    -- XML namespace
    (Maybe Namespace)
    -- Attribute name
    String
    -- Attribute value
    String
  | Property
    -- Property name. Usually is equal to attribute name, exeptions are: "htmlFor" property is a "for" attribute, "className" - "class"
    String
    PropValue
  | Handler
    -- Event type to listen to
    DOM.EventType
    -- Function that builds input for emitter (EmitterInputBuilder), if Nothing is returned - emitter is not called
    -- NOTE: If multiple event handlers are added for the same event for the same element - only last event handler is going to work
    -- (e.g. like in `H.div [HP.eventHandler (...), HP.eventHandler (...)]`)
    (DOM.Event → Maybe a)
  | Ref
    -- This function builds input for emitter function too, but when parent element is removed or created
    -- If Nothing is returned - emitter is not called
    -- NOTE: If multiple ref handlers are added for the same element - only last ref handler is going to work
    (ElemRef DOM.Element → Maybe a)

instance functorProp ∷ Functor Prop where
  map f (Handler ty g) = Handler ty (map f <$> g)
  map f (Ref g) = Ref (map f <$> g)
  map f p = unsafeCoerce p

data ElemRef a
  = Created a
  | Removed a

instance functorElemRef ∷ Functor ElemRef where
  map f (Created a) = Created (f a)
  map f (Removed a) = Removed (f a)

foreign import data PropValue ∷ Type

propFromString ∷ String → PropValue
propFromString = unsafeCoerce

propFromBoolean ∷ Boolean → PropValue
propFromBoolean = unsafeCoerce

propFromInt ∷ Int → PropValue
propFromInt = unsafeCoerce

propFromNumber ∷ Number → PropValue
propFromNumber = unsafeCoerce

type EmitterInputBuilder a = DOM.Event -> Maybe a
type EventListenerAndCurrentEmitterInputBuilder a = Tuple DOM.EventListener (Ref.Ref (EmitterInputBuilder a))

type PropState a =
  { events :: Object.Object (EventListenerAndCurrentEmitterInputBuilder a)
  , props :: Object.Object (Prop a)
  }

-- | A `Machine`` for applying attributes, properties, and event handlers.
-- | An emitter effect must be provided to respond to events. For example,
-- | to allow arbitrary effects in event handlers, one could use `id`.
buildProp
  ∷ ∀ a
  . (a → Effect Unit) -- emitter, for example the global broadcaster function for all elements in halogen component
  → DOM.Element
  → V.Machine (Array (Prop a)) Unit -- Machine takes array of properties for that element, outputs nothing
buildProp emit el = renderProp
  where
  -- what it does - creates a machine, that contains state
  -- on next step - patches prop
  -- on halt - all ref watchers are notified that element is removed

  renderProp :: EFn.EffectFn1 (Array (Prop a)) (Step (Array (Prop a)) Unit)
  renderProp = EFn.mkEffectFn1 \ps1 → do
    (events :: STObject' (EventListenerAndCurrentEmitterInputBuilder a)) ← Util.newMutMap

    -- for each prop in array:
    --   if prop is attr - set attr to element, store attr under "attr/XXX" key in a returned object
    --   if prop is property - set property to element, store property under "prop/XXX" key in a returned object
    --   if prop is handler for DOM.EventType - start listen and add listener to `events` mutable map, store handler under "handler/EVENTTYPE" in a returned object
    --   if prop is ref updater - store `emitterInputBuilder` in under a `ref` key in a returned object, call `emitter` on creation of all props (now) and on halt of all props (later)
    (props :: Object.Object (Prop a)) ← EFn.runEffectFn3 Util.strMapWithIxE ps1 propToStrKey (applyProp events)
    let
      (state :: PropState a) =
        { events: Util.unsafeFreeze events
        , props
        }
    pure $ mkStep $ Step unit state patchProp haltProp

  patchProp ::
    EFn.EffectFn2
    (PropState a)
    (Array (Prop a))
    (Step (Array (Prop a)) Unit)
  patchProp = EFn.mkEffectFn2 \state ps2 → do
    events ← Util.newMutMap
    let
      { events: prevEvents, props: ps1 } = state
      onThese = Fn.runFn2 diffProp prevEvents events
      onThis = removeProp prevEvents
      onThat = applyProp events
    props ← EFn.runEffectFn6 Util.diffWithKeyAndIxE ps1 ps2 propToStrKey onThese onThis onThat
    let
      nextState =
        { events: Util.unsafeFreeze events
        , props
        }
    pure $ mkStep $ Step unit nextState patchProp haltProp

  haltProp
    :: EFn.EffectFn1
    (PropState a)
    Unit
  haltProp = EFn.mkEffectFn1 \state → do
    case Object.lookup "ref" state.props of
      Just (Ref emitterInputBuilder) →
        EFn.runEffectFn1 mbEmit (emitterInputBuilder (Removed el))
      _ → pure unit

  mbEmit :: EFn.EffectFn1 (Maybe a) Unit
  mbEmit = EFn.mkEffectFn1 case _ of
    Just a → emit a
    _ → pure unit

  applyProp
    :: STObject' (EventListenerAndCurrentEmitterInputBuilder a)
    -> EFn.EffectFn3 String Int (Prop a) (Prop a)
  applyProp events = EFn.mkEffectFn3 \_ _ v →
    case v of
      Attribute ns attr val → do
        EFn.runEffectFn4 Util.setAttribute (toNullable ns) attr val el
        pure v
      Property prop val → do
        EFn.runEffectFn3 setProperty prop val el
        pure v
      Handler (DOM.EventType eventType) emitterInputBuilder → do
        case Fn.runFn2 Util.unsafeGetAny eventType events of
          -- if eventType is already present in events storage / listened
          handler | Fn.runFn2 Util.unsafeHasAny eventType events → do
            -- replace current event listener with new
            Ref.write emitterInputBuilder (snd handler)
            pure v
          _ → do
            ref ← Ref.new emitterInputBuilder
            listener ← DOM.eventListener \ev → do
              (emitterInputBuilder' :: EmitterInputBuilder a) ← Ref.read ref
              EFn.runEffectFn1 mbEmit (emitterInputBuilder' ev)

            -- set/add to events map, key is eventType, value contains element listener (so we can remove it on halt) AND current emitterInputBuilder
            EFn.runEffectFn3 Util.pokeMutMap eventType (Tuple listener ref) events

            -- listen events of that type on the element
            EFn.runEffectFn3 Util.addEventListener eventType listener el
            pure v
      Ref emitterInputBuilder → do
        EFn.runEffectFn1 mbEmit (emitterInputBuilder (Created el))
        pure v

  diffProp
    :: Fn.Fn2
    (Object.Object (EventListenerAndCurrentEmitterInputBuilder a))
    (STObject' (EventListenerAndCurrentEmitterInputBuilder a))
    (EFn.EffectFn4 String Int (Prop a) (Prop a) (Prop a))
  diffProp = Fn.mkFn2 \prevEvents events → EFn.mkEffectFn4 \_ _ v1 v2 →
    case v1, v2 of
      Attribute _ _ val1, Attribute ns2 attr2 val2 →
        if val1 == val2
          then pure v2
          else do
            EFn.runEffectFn4 Util.setAttribute (toNullable ns2) attr2 val2 el
            pure v2
      Property _ val1, Property prop2 val2 →
        case Fn.runFn2 Util.refEq val1 val2, prop2 of
          true, _ →
            pure v2
          _, "value" → do
            let elVal = Fn.runFn2 unsafeGetProperty "value" el
            if Fn.runFn2 Util.refEq elVal val2
              then pure v2
              else do
                EFn.runEffectFn3 setProperty prop2 val2 el
                pure v2
          _, _ → do
            EFn.runEffectFn3 setProperty prop2 val2 el
            pure v2
      Handler _ _, Handler (DOM.EventType ty) emitterInputBuilder → do
        let
          handler = Fn.runFn2 Util.unsafeLookup ty prevEvents
        Ref.write emitterInputBuilder (snd handler)
        EFn.runEffectFn3 Util.pokeMutMap ty handler events
        pure v2
      _, _ →
        pure v2

  removeProp :: Object.Object (EventListenerAndCurrentEmitterInputBuilder a) -> EFn.EffectFn2 String (Prop a) Unit
  removeProp prevEvents = EFn.mkEffectFn2 \_ v →
    case v of
      Attribute ns attr _ →
        EFn.runEffectFn3 Util.removeAttribute (toNullable ns) attr el
      Property prop _ →
        EFn.runEffectFn2 removeProperty prop el
      Handler (DOM.EventType ty) _ → do
        let
          handler = Fn.runFn2 Util.unsafeLookup ty prevEvents
        EFn.runEffectFn3 Util.removeEventListener ty (fst handler) el
      Ref _ →
        pure unit

propToStrKey ∷ ∀ i. Prop i → String
propToStrKey = case _ of
  Attribute (Just (Namespace ns)) attr _ → "attr/" <> ns <> ":" <> attr
  Attribute _ attr _ → "attr/:" <> attr
  Property prop _ → "prop/" <> prop
  Handler (DOM.EventType ty) _ → "handler/" <> ty
  Ref _ → "ref"

setProperty ∷ EFn.EffectFn3 String PropValue DOM.Element Unit
setProperty = Util.unsafeSetAny

unsafeGetProperty ∷ Fn.Fn2 String DOM.Element PropValue
unsafeGetProperty = Util.unsafeGetAny

removeProperty ∷ EFn.EffectFn2 String DOM.Element Unit
removeProperty = EFn.mkEffectFn2 \key el →
  EFn.runEffectFn3 Util.hasAttribute (null :: Nullable Namespace) key el >>= if _ -- If attr exists on element
    then EFn.runEffectFn3 Util.removeAttribute (null :: Nullable Namespace) key el -- remove it using el.removeAttribute()
    else case typeOf (Fn.runFn2 Util.unsafeGetAny key el) of
      "string" → EFn.runEffectFn3 Util.unsafeSetAny key "" el -- If it's property - set it to ""
      _        → case key of
        "rowSpan" → EFn.runEffectFn3 Util.unsafeSetAny key 1 el
        "colSpan" → EFn.runEffectFn3 Util.unsafeSetAny key 1 el
        _ → EFn.runEffectFn3 Util.unsafeSetAny key Util.jsUndefined el
