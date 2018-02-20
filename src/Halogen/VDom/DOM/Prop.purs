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
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Ref as Ref
import Data.Maybe (Maybe(..), maybe)
import Data.StrMap as StrMap
import Data.Nullable (toNullable)
import Data.Foreign (typeOf)
import Data.Function.Uncurried as Fn
import Data.Tuple (Tuple(..), fst, snd)
import DOM (DOM)
import DOM.Event.EventTarget (eventListener) as DOM
import DOM.Event.Types (EventType(..), Event) as DOM
import DOM.Node.Types (Element) as DOM
import Halogen.VDom as V
import Halogen.VDom.Types (Namespace(..))
import Halogen.VDom.Util as Util
import Unsafe.Coerce (unsafeCoerce)


-- | Attributes, properties, event handlers, and element lifecycles.
-- | Parameterized by the type of handlers outputs.
data Prop a
  = Attribute (Maybe Namespace) String String
  | Property String PropValue
  | Handler DOM.EventType (DOM.Event → Maybe a)
  | Ref (ElemRef DOM.Element → Maybe a)
  | BHandler String (String → a)

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
  ∷ ∀ eff a
  . (a → Eff (ref ∷ REF, dom ∷ DOM | eff) Unit)
  → DOM.Element
  → V.VDomMachine (ref ∷ REF, dom ∷ DOM | eff) (Array (Prop a)) Unit
buildProp emit el = render
  where
  render ps1 = do
    events ← Util.newMutMap
    ps1' ← Fn.runFn3 Util.strMapWithIxE ps1 propToStrKey (applyProp events)
    pure
      (V.Step unit
        (Fn.runFn2 patch (Util.unsafeFreeze events) ps1')
        (done ps1'))

  patch = Fn.mkFn2 \prevEvents ps1 → \ps2 → do
    events ← Util.newMutMap
    let
      onThese = Fn.runFn2 diffProp prevEvents events
      onThis = removeProp prevEvents
      onThat = applyProp events
    ps2' ← Fn.runFn6 Util.diffWithKeyAndIxE ps1 ps2 propToStrKey onThese onThis onThat
    pure
      (V.Step unit
        (Fn.runFn2 patch (Util.unsafeFreeze events) ps2')
        (done ps2'))

  done ps = do
    case StrMap.lookup "ref" ps of
      Just (Ref f) → do
        mbEmit (f (Removed el))
      _ → do
        Util.effUnit

  mbEmit =
    maybe Util.effUnit emit

  applyProp events = Fn.mkFn3 \_ _ v →
    case v of
      Attribute ns attr val → do
        Fn.runFn4 Util.setAttribute (toNullable ns) attr val el
        pure v
      Property prop val → do
        Fn.runFn3 setProperty prop val el
        pure v
      Handler (DOM.EventType ty) f → do
        case Fn.runFn2 Util.unsafeGetAny ty events of
          handler | Fn.runFn2 Util.unsafeHasAny ty events → do
            Ref.writeRef (snd handler) f
            pure v
          _ → do
            ref ← Ref.newRef f
            let
              listener = DOM.eventListener \ev → do
                f' ← Ref.readRef ref
                mbEmit (f' ev)
            Fn.runFn3 Util.pokeMutMap ty (Tuple listener ref) events
            Fn.runFn3 Util.addEventListener ty listener el
            pure v
      Ref f → do
        mbEmit (f (Created el))
        pure v
      BHandler f _ → do
        Fn.runFn3 setProperty "custom" (propFromString f) el
        pure v

  diffProp = Fn.mkFn2 \prevEvents events → Fn.mkFn4 \_ _ v1 v2 →
    case v1, v2 of
      Attribute _ _ val1, Attribute ns2 attr2 val2 →
        case val1 /= val2 of
          true → do
            Fn.runFn4 Util.setAttribute (toNullable ns2) attr2 val2 el
            pure v2
          _ →
            Util.effPure v2
      Property _ val1, Property prop2 val2 →
        case Fn.runFn2 Util.refEq val1 val2, prop2 of
          true, _ →
            Util.effPure v2
          _, "value" → do
            let elVal = Fn.runFn2 unsafeGetProperty "value" el
            case not (Fn.runFn2 Util.refEq elVal val2) of
              true → do
                Fn.runFn3 setProperty prop2 val2 el
                pure v2
              _ →
                Util.effPure v2
          _, _ → do
            Fn.runFn3 setProperty prop2 val2 el
            pure v2
      Handler _ _, Handler (DOM.EventType ty) f → do
        let
          handler = Fn.runFn2 Util.unsafeLookup ty prevEvents
        Ref.writeRef (snd handler) f
        Fn.runFn3 Util.pokeMutMap ty handler events
        pure v2
      _, _ →
        Util.effPure v2

  removeProp prevEvents = Fn.mkFn2 \_ v →
    case v of
      Attribute ns attr _ →
        Fn.runFn3 Util.removeAttribute (toNullable ns) attr el
      Property prop _ →
        Fn.runFn2 removeProperty prop el
      Handler (DOM.EventType ty) _ → do
        let
          handler = Fn.runFn2 Util.unsafeLookup ty prevEvents
        Fn.runFn3 Util.removeEventListener ty (fst handler) el
      BHandler _ _ → Util.effUnit
      Ref _ →
        Util.effUnit

propToStrKey ∷ ∀ i. Prop i → String
propToStrKey = case _ of
  Attribute (Just (Namespace ns)) attr _ → "attr/" <> ns <> ":" <> attr
  Attribute _ attr _ → "attr/:" <> attr
  Property prop _ → "prop/" <> prop
  Handler (DOM.EventType ty) _ → "handler/" <> ty
  Ref _ → "ref"
  BHandler _ _ -> "bhandler"

setProperty ∷ ∀ eff. Fn.Fn3 String PropValue DOM.Element (Eff (dom ∷ DOM | eff) Unit)
setProperty = Util.unsafeSetAny

unsafeGetProperty ∷ Fn.Fn2 String DOM.Element PropValue
unsafeGetProperty = Util.unsafeGetAny

removeProperty ∷ ∀ eff. Fn.Fn2 String DOM.Element (Eff (dom ∷ DOM | eff) Unit)
removeProperty = Fn.mkFn2 \key el →
  case typeOf (Fn.runFn2 Util.unsafeGetAny key el) of
    "string" → Fn.runFn3 Util.unsafeSetAny key "" el
    _        → case key of
      "rowSpan" → Fn.runFn3 Util.unsafeSetAny key 1 el
      "colSpan" → Fn.runFn3 Util.unsafeSetAny key 1 el
      _ → Fn.runFn3 Util.unsafeSetAny key Util.jsUndefined el
