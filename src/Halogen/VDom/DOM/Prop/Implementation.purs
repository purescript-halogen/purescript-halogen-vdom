module Halogen.VDom.DOM.Prop.Implementation where

import Halogen.VDom.DOM.Prop.Types (ElemRef(..), EmitterInputBuilder, EventListenerAndCurrentEmitterInputBuilder, Prop(..), PropValue)
import Halogen.VDom.DOM.Prop.Utils (removeProperty, setProperty, unsafeGetProperty)
import Prelude (Unit, bind, discard, pure, unit, ($), (<#>), (<>), (==))

import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe, toNullable)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Uncurried as EFn
import Foreign.Object as Object
import Halogen.VDom.Types (ElemName(..), Namespace)
import Halogen.VDom.Util (STObject', anyToString, fullAttributeName, quote)
import Halogen.VDom.Util as Util
import Web.DOM.Element (Element) as DOM
import Web.Event.Event (EventType(..), Event) as DOM
import Web.Event.EventTarget (eventListener, EventListener) as DOM
import Data.String.Common (toLower)
import Effect.Exception (error, throwException)
import Halogen.VDom.Set as Set

checkAttributeExistsAndIsEqual ∷ Maybe Namespace → String → String → DOM.Element → Effect Unit
checkAttributeExistsAndIsEqual maybeNamespace attributeName expectedElementValue element = do
  elementValue ← (EFn.runEffectFn3 Util.getAttribute (toNullable maybeNamespace) attributeName element) <#> toMaybe
  case elementValue of
    Nothing → throwException $ error $ "Expected element to have an attribute " <> quote (fullAttributeName maybeNamespace (ElemName attributeName)) <> " eq to " <> quote expectedElementValue <> ", but it is missing"
    Just elementValue' →
      if elementValue' == expectedElementValue
        then pure unit
        else throwException $ error $ "Expected element to have an attribute " <> quote (fullAttributeName maybeNamespace (ElemName attributeName)) <> " eq to " <> quote expectedElementValue <> ", but it was equal to " <> quote elementValue'

checkPropExistsAndIsEqual ∷ String → PropValue → DOM.Element → Effect Unit
checkPropExistsAndIsEqual propName expectedPropValue el = do
  let propValue = Fn.runFn2 unsafeGetProperty propName el
  if Fn.runFn2 Util.refEq propValue expectedPropValue
    then pure unit
    else do
      throwException $ error $ "Expected element to have a prop " <> quote propName <> " eq to " <> quote (anyToString expectedPropValue) <> ", but it was equal to " <> quote (anyToString propValue)

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
      EFn.runEffectFn2 Set.delete fullAttributeName' extraAttributeNames
      pure v
    Property propName val → do
      checkPropExistsAndIsEqual propName val el
      let fullAttributeName' = toLower propName -- transforms `colSpan` to `colspan`
      EFn.runEffectFn2 Set.delete fullAttributeName' extraAttributeNames
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
