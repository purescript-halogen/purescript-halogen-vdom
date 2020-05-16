module Halogen.VDom.Set where

import Halogen.VDom.DOM.Checkers
import Halogen.VDom.Util
import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn2) as EFn

data Set proxy

foreign import empty ∷ ∀ a . Effect (Set a)

foreign import delete ∷ ∀ a . EFn.EffectFn2 a (Set a) Unit

foreign import add ∷ ∀ a . EFn.EffectFn2 a (Set a) Unit

foreign import size ∷ ∀ a . Set a → Int

foreign import toArray ∷ ∀ a . Set a → Array a
