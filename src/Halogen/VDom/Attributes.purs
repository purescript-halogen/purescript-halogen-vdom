module Halogen.VDom.Attributes where

import Prelude

import Halogen.VDom.Util
import Halogen.VDom.DOM.Checkers
import Effect (Effect)
import Web.DOM.Element as DOM
import Effect.Uncurried as EFn
import Unsafe.Coerce (unsafeCoerce)
import Halogen.VDom.Util as Util

data NamedNodeMap

foreign import attributes ∷ DOM.Element → NamedNodeMap

forEachE
  ∷ ∀ a
  . EFn.EffectFn2
      NamedNodeMap
      (EFn.EffectFn1 String Unit)
      Unit
forEachE = unsafeCoerce Util.forEachE
