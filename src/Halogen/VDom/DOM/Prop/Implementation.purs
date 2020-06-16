module Halogen.VDom.DOM.Prop.Implementation where

import Prelude

import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.String (joinWith)
import Data.String.Common (toLower)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Effect.Ref as Ref
import Effect.Uncurried as EFn
import Foreign.Object as Object
import Halogen.VDom.DOM.Prop.Checkers (checkAttributeExistsAndIsEqual, checkPropExistsAndIsEqual)
import Halogen.VDom.DOM.Prop.Types (ElemRef(..), EmitterInputBuilder, EventListenerAndCurrentEmitterInputBuilder, Prop(..))
import Halogen.VDom.DOM.Prop.Utils (removeProperty, setProperty, unsafeGetProperty)
import Halogen.VDom.Set as Set
import Halogen.VDom.Types (ElemName(..))
import Halogen.VDom.Util (STObject', fullAttributeName, quote)
import Halogen.VDom.Util as Util
import Web.DOM.Element (Element) as DOM
import Web.Event.Event (EventType(..), Event) as DOM
import Web.Event.EventTarget (eventListener, EventListener) as DOM

deleteRequiredElement :: EFn.EffectFn2 String (Set.Set String) Unit
deleteRequiredElement = EFn.mkEffectFn2 \element set -> do
  let isPresent = Fn.runFn2 Set.has element set
  if isPresent
    then EFn.runEffectFn2 Set.delete element set
    else throwException $ error $ "Cannot delete element that is not present in set, " <> quote element <> " should be present in set" <> (Set.toArray set # joinWith ", ")

hydrateApplyProp
  ∷ ∀ a
  . Fn.Fn4
  (Set.Set String)
  DOM.Element
  (a → Effect Unit)
  (STObject' (EventListenerAndCurrentEmitterInputBuilder a))
  (EFn.EffectFn3 String Int (Prop a) (Prop a))
hydrateApplyProp = Fn.mkFn4 \extraAttributeNames el emit events → EFn.mkEffectFn3 \_ _ v →
  case v of
    Attribute maybeNamespace attributeName val → do
      checkAttributeExistsAndIsEqual maybeNamespace attributeName val el
      let fullAttributeName' = fullAttributeName maybeNamespace (ElemName attributeName) -- should be lowercased
      EFn.runEffectFn2 deleteRequiredElement fullAttributeName' extraAttributeNames
      pure v
    Property propName val → do
      checkPropExistsAndIsEqual propName val el

      -- | EFn.runEffectFn2 warnAny "checkPropExistsAndIsEqual" { propName, val, el, extraAttributeNames }

      if propName == "className"
        then EFn.runEffectFn2 deleteRequiredElement "class" extraAttributeNames
        else do
          let fullAttributeName' = toLower propName -- transforms `colSpan` to `colspan`
          EFn.runEffectFn2 deleteRequiredElement fullAttributeName' extraAttributeNames

      -- | EFn.runEffectFn2 warnAny "checkPropExistsAndIsEqual after" { propName, val, el, extraAttributeNames }

      pure v
    Handler eventType emitterInputBuilder → do
      EFn.runEffectFn5 applyPropHandler el emit events eventType emitterInputBuilder
      pure v
    Ref emitterInputBuilder → do
      EFn.runEffectFn2 mbEmit emit (emitterInputBuilder (Created el))
      pure v

applyProp
  ∷ ∀ a
  . Fn.Fn3
  DOM.Element
  (a → Effect Unit)
  (STObject' (EventListenerAndCurrentEmitterInputBuilder a))
  (EFn.EffectFn3 String Int (Prop a) (Prop a))
applyProp = Fn.mkFn3 \el emit events → EFn.mkEffectFn3 \_ _ v →
  case v of
    Attribute maybeNamespace attributeName val → do
      EFn.runEffectFn4 Util.setAttribute (toNullable maybeNamespace) attributeName val el
      pure v
    Property propName val → do
      EFn.runEffectFn3 setProperty propName val el
      pure v
    Handler eventType emitterInputBuilder → do
      EFn.runEffectFn5 applyPropHandler el emit events eventType emitterInputBuilder
      pure v
    Ref emitterInputBuilder → do
      EFn.runEffectFn2 mbEmit emit (emitterInputBuilder (Created el))
      pure v

applyPropHandler
  ∷ ∀ a
  . EFn.EffectFn5
  DOM.Element
  (a -> Effect Unit)
  (STObject' (Tuple DOM.EventListener (Ref.Ref (DOM.Event -> Maybe a))))
  DOM.EventType
  (DOM.Event -> Maybe a)
  Unit
applyPropHandler = EFn.mkEffectFn5 \el emit events (DOM.EventType eventType) emitterInputBuilder →
  case Fn.runFn2 Util.unsafeGetAny eventType events of
    -- if eventType is already present in events storage / listened
    handler | Fn.runFn2 Util.unsafeHasAny eventType events → do
      -- replace current event listener with new
      Ref.write emitterInputBuilder (snd handler)
    _ → do
      ref ← Ref.new emitterInputBuilder
      listener ← DOM.eventListener \ev → do
        (emitterInputBuilder' ∷ EmitterInputBuilder a) ← Ref.read ref
        EFn.runEffectFn2 mbEmit emit (emitterInputBuilder' ev)

      -- set/add to events map, key is eventType, value contains element listener (so we can remove it on halt) AND current emitterInputBuilder
      EFn.runEffectFn3 Util.pokeMutMap eventType (Tuple listener ref) events

      -- listen events of that type on the element
      EFn.runEffectFn3 Util.addEventListener eventType listener el

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
        -- | In many browsers, though it may not be the case anymore, setting the input value always resets the cursor position/selection.
        -- | This avoids setting it if it has not changed so as not to reset the cursor when you are typing.
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
    Attribute maybeNamespace attributeName _ →
      EFn.runEffectFn3 Util.removeAttribute (toNullable maybeNamespace) attributeName el
    Property propName _ →
      EFn.runEffectFn2 removeProperty propName el
    Handler (DOM.EventType ty) _ → do
      let
        handler = Fn.runFn2 Util.unsafeLookup ty prevEvents
      EFn.runEffectFn3 Util.removeEventListener ty (fst handler) el
    Ref _ →
      pure unit
