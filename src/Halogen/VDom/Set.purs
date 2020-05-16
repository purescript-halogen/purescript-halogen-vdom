module Halogen.VDom.Set where

import Halogen.VDom.DOM.Checkers
import Halogen.VDom.Util
import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn2) as EFn

data Set proxy

foreign import mkSet ∷ ∀ a . Effect (Set a)

foreign import removeSetMember ∷ ∀ a . EFn.EffectFn2 a (Set a) Unit

foreign import addSetMember ∷ ∀ a . EFn.EffectFn2 a (Set a) Unit

foreign import setSize ∷ ∀ a . Set a → Int

foreign import setToArray ∷ ∀ a . Set a → Array a
