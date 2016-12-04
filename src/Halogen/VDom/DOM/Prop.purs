module Halogen.VDom.DOM.Prop
  ( Prop(..)
  , Namespace(..)
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
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.StrMap.ST (STStrMap)
import Data.StrMap.ST as STStrMap
import Data.Nullable (Nullable, toNullable)
import Data.Function.Uncurried as Fn
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..), fst, snd)
import DOM (DOM)
import DOM.Event.EventTarget (EventListener, eventListener) as DOM
import DOM.Event.Types (EventType(..), Event) as DOM
import DOM.HTML.Types (HTMLElement) as DOM
import DOM.Node.Types (Element) as DOM
import Halogen.VDom as V
import Halogen.VDom.Util (refEq, diffWithKeyAndIxE, strMapWithIxE)
import Unsafe.Coerce (unsafeCoerce)

data Prop a
  = Attribute (Maybe Namespace) String String
  | Property String PropValue
  | Handler DOM.EventType (DOM.Event → Maybe a)
  | Ref (ElemRef DOM.HTMLElement → Maybe a)

instance functorProp ∷ Functor Prop where
  map f (Handler ty g) = Handler ty (map f <$> g)
  map f (Ref g) = Ref (map f <$> g)
  map f p = unsafeCoerce p

newtype Namespace = Namespace String

derive instance newtypeNamespace ∷ Newtype Namespace _

data ElemRef a
  = Created a
  | Removed a

instance functorElemRef ∷ Functor ElemRef where
  map f (Created a) = Created (f a)
  map f (Removed a) = Removed (f a)

foreign import data PropValue ∷ *

propFromString ∷ String → PropValue
propFromString = unsafeCoerce

propFromBoolean ∷ Boolean → PropValue
propFromBoolean = unsafeCoerce

propFromInt ∷ Int → PropValue
propFromInt = unsafeCoerce

propFromNumber ∷ Number → PropValue
propFromNumber = unsafeCoerce

type PropEff eff a =
  Eff (dom ∷ DOM, ref ∷ REF | eff) a

buildProp
  ∷ ∀ eff a
  . (a → Eff (ref ∷ REF, dom ∷ DOM | eff) Unit)
  → DOM.Element
  → V.VDomMachine (ref ∷ REF, dom ∷ DOM | eff) (Array (Prop a)) Unit
buildProp emit el = render
  where
  render ps1 = do
    events ← newMutMap
    ps1' ← Fn.runFn3 strMapWithIxE ps1 propToStrKey (applyProp events)
    pure
      (V.Step unit
        (Fn.runFn2 patch (unsafeFreeze events) ps1')
        (done ps1'))

  patch = Fn.mkFn2 \prevEvents ps1 → \ps2 → do
    events ← newMutMap
    let
      onThese = Fn.runFn2 diffProp prevEvents events
      onThis = removeProp prevEvents
      onThat = applyProp events
    ps2' ← Fn.runFn6 diffWithKeyAndIxE ps1 ps2 propToStrKey onThese onThis onThat
    pure
      (V.Step unit
        (Fn.runFn2 patch (unsafeFreeze events) ps2')
        (done ps2'))

  done ps = do
    case StrMap.lookup "ref" ps of
      Just (Ref f) → do
        mbEmit (f (Removed (unsafeElementToHTMLElement el)))
      _ → do
        effUnit

  mbEmit =
    maybe effUnit emit

  applyProp events = Fn.mkFn3 \_ _ v →
    case v of
      Attribute ns attr val → do
        Fn.runFn4 setAttribute (toNullable ns) attr val el
        pure v
      Property prop val → do
        Fn.runFn3 setProperty prop val el
        pure v
      Handler (DOM.EventType ty) f → do
        ref ← Ref.newRef f
        let
          listener = DOM.eventListener \ev → do
            f' ← Ref.readRef ref
            mbEmit (f' ev)
        Fn.runFn3 pokeMutMap ty (Tuple listener ref) events
        Fn.runFn3 addEventListener ty listener el
        pure v
      Ref f → do
        mbEmit (f (Created (unsafeElementToHTMLElement el)))
        pure v

  diffProp = Fn.mkFn2 \prevEvents events → Fn.mkFn4 \_ _ v1 v2 →
    case v1, v2 of
      Attribute _ _ val1, Attribute ns2 attr2 val2 →
        case val1 /= val2 of
          true → do
            Fn.runFn4 setAttribute (toNullable ns2) attr2 val2 el
            pure v2
          _ →
            effPure v2
      Property _ val1, Property prop2 val2 →
        case Fn.runFn2 refEq val1 val2, prop2 of
          true, _ →
            effPure v2
          _, "value" → do
            elVal ← Fn.runFn2 getProperty "value" el
            case not (Fn.runFn2 refEq elVal val2) of
              true → do
                Fn.runFn3 setProperty prop2 val2 el
                pure v2
              _ →
                pure v2
          _, _ → do
            Fn.runFn3 setProperty prop2 val2 el
            pure v2
      Handler _ _, Handler (DOM.EventType ty) f → do
        let
          handler = Fn.runFn2 unsafeLookup ty prevEvents
        Ref.writeRef (snd handler) f
        Fn.runFn3 pokeMutMap ty handler events
        pure v2
      _, _ →
        effPure v2

  removeProp prevEvents = Fn.mkFn2 \_ v →
    case v of
      Attribute ns attr _ →
        Fn.runFn3 removeAttribute (toNullable ns) attr el
      Property prop _ →
        Fn.runFn2 removeProperty prop el
      Handler (DOM.EventType ty) _ → do
        let
          handler = Fn.runFn2 unsafeLookup ty prevEvents
        Fn.runFn3 removeEventListener ty (fst handler) el
      Ref _ →
        effUnit

propToStrKey ∷ ∀ i. Prop i → String
propToStrKey = case _ of
  Attribute (Just (Namespace ns)) attr _ → "attr/" <> ns <> ":" <> attr
  Attribute _ attr _ → "attr/:" <> attr
  Property prop _ → "prop/" <> prop
  Handler (DOM.EventType ty) _ → "handler/" <> ty
  Ref _ → "ref"

unsafeElementToHTMLElement ∷ DOM.Element → DOM.HTMLElement
unsafeElementToHTMLElement = unsafeCoerce

type MutStrMap = STStrMap Void

newMutMap ∷ ∀ eff a. Eff (ref ∷ REF | eff) (MutStrMap a)
newMutMap = unsafeCoerce STStrMap.new

unsafeFreeze ∷ ∀ a. MutStrMap a → StrMap a
unsafeFreeze = unsafeCoerce

-- To avoid dictionary passing in some cases
effPure ∷ ∀ eff a. a → Eff eff a
effPure = pure

effUnit ∷ ∀ eff. Eff eff Unit
effUnit = pure unit

foreign import setAttribute
  ∷ ∀ eff
  . Fn.Fn4 (Nullable Namespace) String String DOM.Element (Eff (dom ∷ DOM | eff) Unit)

foreign import removeAttribute
  ∷ ∀ eff
  . Fn.Fn3 (Nullable Namespace) String DOM.Element (Eff (dom ∷ DOM | eff) Unit)

foreign import addEventListener
  ∷ ∀ eff
  . Fn.Fn3 String (DOM.EventListener (dom ∷ DOM | eff)) DOM.Element (Eff (dom ∷ DOM | eff) Unit)

foreign import removeEventListener
  ∷ ∀ eff
  . Fn.Fn3 String (DOM.EventListener (dom ∷ DOM | eff)) DOM.Element (Eff (dom ∷ DOM | eff) Unit)

foreign import setProperty
  ∷ ∀ eff
  . Fn.Fn3 String PropValue DOM.Element (Eff (dom ∷ DOM | eff) Unit)

foreign import getProperty
  ∷ ∀ eff
  . Fn.Fn2 String DOM.Element (Eff (dom ∷ DOM | eff) PropValue)

foreign import removeProperty
  ∷ ∀ eff
  . Fn.Fn2 String DOM.Element (Eff (dom ∷ DOM | eff) Unit)

foreign import unsafeLookup
  ∷ ∀ a. Fn.Fn2 String (StrMap a) a

foreign import pokeMutMap
  ∷ ∀ eff a
  . Fn.Fn3 String a (MutStrMap a) (Eff (ref ∷ REF | eff) Unit)
