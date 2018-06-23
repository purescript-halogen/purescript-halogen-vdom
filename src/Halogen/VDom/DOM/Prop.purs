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
import Data.Nullable (toNullable)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Uncurried as EFn
import Foreign (typeOf)
import Foreign.Object as Object
import Halogen.VDom as V
import Halogen.VDom.Machine (Step'(..), mkStep)
import Halogen.VDom.Types (Namespace(..))
import Halogen.VDom.Util as Util
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element) as DOM
import Web.Event.Event (EventType(..), Event) as DOM
import Web.Event.EventTarget (eventListener) as DOM

-- | Attributes, properties, event handlers, and element lifecycles.
-- | Parameterized by the type of handlers outputs.
data Prop a
  = Attribute (Maybe Namespace) String String
  | Property String PropValue
  | Handler DOM.EventType (DOM.Event → Maybe a)
  | Ref (ElemRef DOM.Element → Maybe a)

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

-- | A `Machine`` for applying attributes, properties, and event handlers.
-- | An emitter effect must be provided to respond to events. For example,
-- | to allow arbitrary effects in event handlers, one could use `id`.
buildProp
  ∷ ∀ a
  . (a → Effect Unit)
  → DOM.Element
  → V.Machine (Array (Prop a)) Unit
buildProp emit el = renderProp
  where
  renderProp = EFn.mkEffectFn1 \ps1 → do
    events ← Util.newMutMap
    ps1' ← EFn.runEffectFn3 Util.strMapWithIxE ps1 propToStrKey (applyProp events)
    let
      state =
        { events: Util.unsafeFreeze events
        , props: ps1'
        }
    pure $ mkStep $ Step unit state patchProp haltProp

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

  haltProp = EFn.mkEffectFn1 \state → do
    case Object.lookup "ref" state.props of
      Just (Ref f) →
        EFn.runEffectFn1 mbEmit (f (Removed el))
      _ → pure unit

  mbEmit = EFn.mkEffectFn1 case _ of
    Just a → emit a
    _ → pure unit

  applyProp events = EFn.mkEffectFn3 \_ _ v →
    case v of
      Attribute ns attr val → do
        EFn.runEffectFn4 Util.setAttribute (toNullable ns) attr val el
        pure v
      Property prop val → do
        EFn.runEffectFn3 setProperty prop val el
        pure v
      Handler (DOM.EventType ty) f → do
        case Fn.runFn2 Util.unsafeGetAny ty events of
          handler | Fn.runFn2 Util.unsafeHasAny ty events → do
            Ref.write f (snd handler)
            pure v
          _ → do
            ref ← Ref.new f
            listener ← DOM.eventListener \ev → do
              f' ← Ref.read ref
              EFn.runEffectFn1 mbEmit (f' ev)
            EFn.runEffectFn3 Util.pokeMutMap ty (Tuple listener ref) events
            EFn.runEffectFn3 Util.addEventListener ty listener el
            pure v
      Ref f → do
        EFn.runEffectFn1 mbEmit (f (Created el))
        pure v

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
      Handler _ _, Handler (DOM.EventType ty) f → do
        let
          handler = Fn.runFn2 Util.unsafeLookup ty prevEvents
        Ref.write f (snd handler)
        EFn.runEffectFn3 Util.pokeMutMap ty handler events
        pure v2
      _, _ →
        pure v2

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
  case typeOf (Fn.runFn2 Util.unsafeGetAny key el) of
    "string" → EFn.runEffectFn3 Util.unsafeSetAny key "" el
    _        → case key of
      "rowSpan" → EFn.runEffectFn3 Util.unsafeSetAny key 1 el
      "colSpan" → EFn.runEffectFn3 Util.unsafeSetAny key 1 el
      _ → EFn.runEffectFn3 Util.unsafeSetAny key Util.jsUndefined el
