module Halogen.VDom.Machine
  ( Machine
  , Step(..)
  , extract
  , step
  , halt
  ) where

import Prelude

import Effect (Effect)
import Effect.Uncurried (EffectFn1)

type Machine a b = EffectFn1 a (Step a b)

data Step a b = Step b (Machine a b) (Effect Unit)

-- | Returns the output value of a `Step`.
extract ∷ ∀ a b. Step a b → b
extract (Step x _ _) = x

-- | Runs the next step.
step ∷ ∀ a b. Step a b → EffectFn1 a (Step a b)
step (Step _ m _) = m

-- | Runs the finalizer associated with a `Step`
halt ∷ ∀ a b. Step a b → Effect Unit
halt (Step _ _ h) = h
