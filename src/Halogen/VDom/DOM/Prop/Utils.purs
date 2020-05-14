module Halogen.VDom.DOM.Prop.Utils where

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
  EFn.runEffectFn3 Util.hasAttribute (null ∷ Nullable Namespace) key el >>= if _ -- If attr exists on element
    then EFn.runEffectFn3 Util.removeAttribute (null ∷ Nullable Namespace) key el -- remove it using el.removeAttribute()
    else case typeOf (Fn.runFn2 Util.unsafeGetAny key el) of
      "string" → EFn.runEffectFn3 Util.unsafeSetAny key "" el -- If it's property - set it to ""
      _        → case key of
        "rowSpan" → EFn.runEffectFn3 Util.unsafeSetAny key 1 el
        "colSpan" → EFn.runEffectFn3 Util.unsafeSetAny key 1 el
        _ → EFn.runEffectFn3 Util.unsafeSetAny key Util.jsUndefined el
