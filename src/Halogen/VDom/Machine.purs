module Halogen.VDom.Machine
  ( Machine
  , Step(..)
  , extract
  , step
  , halt
  , fold
  , loop
  , stepper
  , constantly
  , never
  ) where

import Prelude
import Data.Tuple (Tuple(..))

type Machine m a b = a → m (Step m a b)

data Step m a b = Step b (Machine m a b) (m Unit)

instance functorStep ∷ Functor m ⇒ Functor (Step m a) where
  map f (Step x m d) = Step (f x) (m >>> map (map f)) d

-- | Returns the output value of a `Step`.
extract ∷ ∀ m a b. Step m a b → b
extract (Step x _ _) = x

-- | Runs the next step.
step ∷ ∀ m a b. Step m a b → a → m (Step m a b)
step (Step _ m _) = m

-- | Runs the finalizer associated with a `Step`
halt ∷ ∀ m a b. Step m a b → m Unit
halt (Step _ _ h) = h

fold ∷ ∀ m a b s. Applicative m ⇒ (s → a → m (Tuple s b)) → (s → m Unit) → s → Machine m a b
fold f g s a = next <$> f s a
  where
  next (Tuple s' b) = Step b (fold f g s') (g s')

loop ∷ ∀ m a s. Applicative m ⇒ (s → a → m s) → (s → m Unit) → s → Machine m a Unit
loop f g s a = next <$> f s a
  where
  next s' = Step unit (loop f g s') (g s')

stepper ∷ ∀ m a b. Functor m ⇒ (a → m b) → m Unit → Machine m a b
stepper f h a = next <$> f a
  where
  next b = Step b (stepper f h) h

constantly ∷ ∀ m a b. Applicative m ⇒ b → Machine m a b
constantly x _ = pure (Step x (constantly x) (pure unit))

never ∷ ∀ m b. Applicative m ⇒ Machine m Void b
never a = pure (Step (absurd a) never (pure unit))
