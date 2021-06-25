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

import Data.Array
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
import Halogen.VDom.Types (Namespace(..), FnObject)
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
  | BHandler String (Unit -> Maybe a)
  | Payload String
  | Nopatch String PropValue
  | ListData (Array (Object.Object PropValue))

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
  → FnObject
  → DOM.Element
  → V.Machine (Array (Prop a)) Unit
buildProp emit fnObject el = renderProp
  where
  renderProp = EFn.mkEffectFn1 \ps1 → do
    events ← Util.newMutMap
    props ← Util.newMutMap
    listData ← Util.newMutMap
    ps1' ← EFn.runEffectFn3 Util.strMapWithIxE ps1 propToStrKey (applyProp "render" events)
    let
      state =
        { events: Util.unsafeFreeze events
        , props: ps1'
        , listData : Util.unsafeFreeze listData
        }
    pure $ mkStep $ Step unit state patchProp haltProp

  patchProp = EFn.mkEffectFn2 \state ps2 → do
    events ← Util.newMutMap
    let
      { events: prevEvents, props: ps1, listData : prevListData } = state
      onThese = Fn.runFn3 diffProp prevEvents events prevListData
      onThis = removeProp prevEvents
      onThat = applyProp "patch" events
    props ← EFn.runEffectFn8 Util.diffPropWithKeyAndIxE fnObject ps1 ps2 propToStrKey onThese onThis onThat el
    let
      nextState =
        { events: Util.unsafeFreeze events
        , props
        , listData : prevListData
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

  applyProp pr events = EFn.mkEffectFn4 \_ _ props v →
    case v of
      Attribute ns attr val → do
        EFn.runEffectFn4 Util.setAttribute (toNullable ns) attr val el
        pure v
      Property prop val → do
        case pr of
             "render" -> EFn.runEffectFn3 setProperty prop val el
             _ -> EFn.runEffectFn3 Util.unsafeSetAny prop val props
        pure v
      Nopatch prop val → do
        case pr of
             "render" -> EFn.runEffectFn3 setProperty prop val el
             _ -> EFn.runEffectFn3 Util.unsafeSetAny prop val props
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
            EFn.runEffectFn5 Util.addEventListener fnObject pr ty listener el
            pure v
      Ref f → do
        EFn.runEffectFn1 mbEmit (f (Created el))
        pure v
      BHandler ty behavior → do
         EFn.runEffectFn1 mbEmit (behavior unit)
         pure v
      Payload payload -> do
        _ <- case pr of
          "render" -> EFn.runEffectFn3 updateMicroAppPayload payload el false
          _ -> EFn.runEffectFn3 updateMicroAppPayload payload el true
            -- TODO ADD UPDATE_ORDER :: THIS LOOKS USELESS TO BE HONEST
        pure v
      ListData ld -> do
        -- Create Run In UI for all props and add to some sort of state
        -- _ <- case pr of
        --   "render" -> 
                -- Call setProp with the final recieved value
        --   _ -> 
                -- Call unsafeSetAny with the final recieved value
        _ <- case pr of
          "render" -> EFn.runEffectFn3 setProperty "listData" (unsafeCoerce ld) el
          _ -> EFn.runEffectFn3 Util.unsafeSetAny "listData" (unsafeCoerce ld) props
        pure v

  diffProp = Fn.mkFn3 \prevEvents events listState → EFn.mkEffectFn5 \_ _ props v1 v2 →
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
                EFn.runEffectFn3 Util.unsafeSetAny prop2 val2 props
                pure v2
          _, _ → do
            EFn.runEffectFn3 Util.unsafeSetAny prop2 val2 props
            pure v2
      Payload val1, Payload val2 →
        if Fn.runFn2 Util.refEq val1 val2
          then pure v2
          else do
            EFn.runEffectFn3 Util.unsafeSetAny "payload" val2 props
            EFn.runEffectFn3 updateMicroAppPayload val2 el true
            pure v2
      Handler _ _, Handler (DOM.EventType ty) f → do
        let
          handler = Fn.runFn2 Util.unsafeLookup ty prevEvents
        Ref.write f (snd handler)
        EFn.runEffectFn3 Util.pokeMutMap ty handler events
        pure v2
      ListData ld1, ListData ld2 -> do
        -- diff;
          -- Call parseParams for new props
          -- Call removeProp for old props
          -- Merge all runInUI props
        EFn.runEffectFn6 Util.diffArrayOfObjects fnObject listState el ld1 ld2 props
        pure v2
      _, _ →
        pure v2

  removeProp prevEvents = EFn.mkEffectFn2 \_ v →
    case v of
      Attribute ns attr _ →
        EFn.runEffectFn3 Util.removeAttribute (toNullable ns) attr el
      Property prop _ →
        EFn.runEffectFn2 removeProperty prop el
      Nopatch prop _ →
        EFn.runEffectFn2 removeProperty prop el
      ListData _ →
        EFn.runEffectFn2 removeProperty "listData" el
      Handler (DOM.EventType ty) _ → do
        let
          handler = Fn.runFn2 Util.unsafeLookup ty prevEvents
        EFn.runEffectFn3 Util.removeEventListener ty (fst handler) el
      Ref _ →
        pure unit
      BHandler ty _ → do
         _ <- EFn.runEffectFn1 fnObject.cancelBehavior ty
         pure unit
      Payload _ -> pure unit

  updateMicroAppPayload ∷ EFn.EffectFn3 String DOM.Element Boolean Unit
  updateMicroAppPayload = fnObject.updateMicroAppPayload

propToStrKey ∷ ∀ i. Prop i → String
propToStrKey = case _ of
  Attribute (Just (Namespace ns)) attr _ → "attr/" <> ns <> ":" <> attr
  Attribute _ attr _ → "attr/:" <> attr
  Property prop _ → "prop/" <> prop
  Nopatch prop _ → "prop/" <> prop
  Handler (DOM.EventType ty) _ → "handler/" <> ty
  Ref _ → "ref"
  BHandler ty _ -> "bhandler/" <> ty
  Payload _ -> "payload"
  ListData _ → "prop/listData"

setProperty ∷ EFn.EffectFn3 String PropValue DOM.Element Unit
setProperty = Util.unsafeSetProp

unsafeGetProperty ∷ Fn.Fn2 String DOM.Element PropValue
unsafeGetProperty = Util.unsafeGetProp

removeProperty ∷ EFn.EffectFn2 String DOM.Element Unit
removeProperty = EFn.mkEffectFn2 \key el →
  case typeOf (Fn.runFn2 Util.unsafeGetProp key el) of
    "string" → EFn.runEffectFn3 Util.removeProperty key "" el
    _        → case key of
      "rowSpan" → EFn.runEffectFn3 Util.unsafeSetAny key 1 el
      "colSpan" → EFn.runEffectFn3 Util.unsafeSetAny key 1 el
      _ → EFn.runEffectFn3 Util.removeProperty key Util.jsUndefined el
