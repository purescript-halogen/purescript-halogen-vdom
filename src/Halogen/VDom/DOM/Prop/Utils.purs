module Halogen.VDom.DOM.Prop.Utils where

import Prelude (Unit, (<>), (>>=))

import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, null)
import Effect.Uncurried as EFn
import Foreign (typeOf)
import Halogen.VDom.Types (Namespace(..))
import Halogen.VDom.Util as Util
import Web.DOM.Element (Element) as DOM
import Web.Event.Event (EventType(..)) as DOM
import Halogen.VDom.DOM.Prop.Types (Prop(..), PropValue)

propToStrKey ∷ ∀ i. Prop i → String
propToStrKey = case _ of
  Attribute (Just (Namespace ns)) attr _ → "attr/" <> ns <> ":" <> attr
  Attribute _ attr _ → "attr/:" <> attr
  Property prop _ → "prop/" <> prop
  Handler (DOM.EventType ty) _ → "handler/" <> ty
  Ref _ → "ref"

setProperty ∷ EFn.EffectFn3 String PropValue DOM.Element Unit
setProperty = Util.unsafeSetAny

unsafeGetProperty ∷ Fn.Fn2 String DOM.Element (Nullable PropValue)
unsafeGetProperty = Util.unsafeGetAny

removeProperty ∷ EFn.EffectFn2 String DOM.Element Unit
removeProperty = EFn.mkEffectFn2 \key el →
  EFn.runEffectFn3 Util.hasAttribute (null ∷ Nullable Namespace) key el >>= if _ -- If attr exists on element
    then EFn.runEffectFn3 Util.removeAttribute (null ∷ Nullable Namespace) key el -- remove it using el.removeAttribute()
    else case typeOf (Fn.runFn2 Util.unsafeGetAny key el) of -- If it's property - set to following
      "string" → EFn.runEffectFn3 Util.unsafeSetAny key "" el
      _        → case key of
        "rowSpan" → EFn.runEffectFn3 Util.unsafeSetAny key 1 el
        "colSpan" → EFn.runEffectFn3 Util.unsafeSetAny key 1 el
        _ → EFn.runEffectFn3 Util.unsafeSetAny key Util.jsUndefined el
