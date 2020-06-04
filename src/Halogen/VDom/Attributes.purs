module Halogen.VDom.Attributes where

import Prelude (Unit)

import Web.DOM.Element as DOM
import Effect.Uncurried as EFn
import Unsafe.Coerce (unsafeCoerce)
import Halogen.VDom.Util as Util

data NamedNodeMap

foreign import attributes ∷ DOM.Element → NamedNodeMap

forEachE
  ∷ EFn.EffectFn2
      NamedNodeMap
      (EFn.EffectFn1 { name :: String } Unit)
      Unit
forEachE = unsafeCoerce Util.forEachE
