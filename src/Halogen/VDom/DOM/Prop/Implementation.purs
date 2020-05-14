module Halogen.VDom.DOM.Prop.Implementation where

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
import Halogen.VDom.DOM.Prop.Types
import Halogen.VDom.DOM.Prop.Utils

applyProp
  ∷ ∀ a
  . Fn.Fn3
  DOM.Element
  (a → Effect Unit)
  (STObject' (EventListenerAndCurrentEmitterInputBuilder a))
  (EFn.EffectFn3 String Int (Prop a) (Prop a))
applyProp = Fn.mkFn3 \el emit events → EFn.mkEffectFn3 \_ _ v →
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
            (emitterInputBuilder' ∷ EmitterInputBuilder a) ← Ref.read ref
            EFn.runEffectFn2 mbEmit emit (emitterInputBuilder' ev)

          -- set/add to events map, key is eventType, value contains element listener (so we can remove it on halt) AND current emitterInputBuilder
          EFn.runEffectFn3 Util.pokeMutMap eventType (Tuple listener ref) events

          -- listen events of that type on the element
          EFn.runEffectFn3 Util.addEventListener eventType listener el
          pure v
    Ref emitterInputBuilder → do
      EFn.runEffectFn2 mbEmit emit (emitterInputBuilder (Created el))
      pure v

mbEmit
  ∷ ∀ a
  . EFn.EffectFn2
  (a → Effect Unit)
  (Maybe a)
  Unit
mbEmit = EFn.mkEffectFn2 \emit ma → case ma of
  Just a → emit a
  _ → pure unit

diffProp
  ∷ ∀ a
  . Fn.Fn3
  DOM.Element
  (Object.Object (EventListenerAndCurrentEmitterInputBuilder a))
  (STObject' (EventListenerAndCurrentEmitterInputBuilder a))
  (EFn.EffectFn4 String Int (Prop a) (Prop a) (Prop a))
diffProp = Fn.mkFn3 \el prevEvents events → EFn.mkEffectFn4 \_ _ v1 v2 →
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

removeProp
  ∷ ∀ a
  . Fn.Fn2
  DOM.Element
  (Object.Object (EventListenerAndCurrentEmitterInputBuilder a))
  (EFn.EffectFn2 String (Prop a) Unit)
removeProp = Fn.mkFn2 \el prevEvents → EFn.mkEffectFn2 \_ v →
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
