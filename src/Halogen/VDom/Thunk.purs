module Halogen.VDom.Thunk
  ( Thunk
  , buildThunk
  , runThunk
  , unsafeEqThunk
  , thunk1
  , thunk2
  , thunk3
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried as Fn
import Halogen.VDom as V
import Halogen.VDom.Util as Util
import Unsafe.Coerce (unsafeCoerce)

foreign import data TArg ∷ Type

foreign import data Thunk ∷ (Type → Type) → Type → Type

data Thunk' f j i = Thunk (f i → f j) (Fn.Fn2 TArg TArg Boolean) (TArg → f i) TArg

mkThunk ∷ ∀ f j i. Thunk' f j i → Thunk f j
mkThunk = unsafeCoerce

unThunk ∷ ∀ f j r. (∀ i. Thunk' f j i → r) → Thunk f j → r
unThunk = unsafeCoerce

unThunk2 ∷ ∀ f j r. (∀ h i. Fn.Fn2 (Thunk' f j h) (Thunk' f j i) r) → Fn.Fn2 (Thunk f j) (Thunk f j) r
unThunk2 = unsafeCoerce

instance functorThunk ∷ Functor f ⇒ Functor (Thunk f) where
  map f = unThunk \(Thunk g r c a1) → mkThunk (Thunk (g >>> map f) r c a1)

thunk ∷ ∀ a f i. Fn.Fn3 (Fn.Fn2 a a Boolean) (a → f i) a (Thunk f i)
thunk = Fn.mkFn3 \comp f a → mkThunk $ Thunk id (unsafeCoerce comp) (unsafeCoerce f) (unsafeCoerce a)

thunk1 ∷ ∀ a f i. Fn.Fn2 (a → f i) a (Thunk f i)
thunk1 = Fn.mkFn2 \f a → Fn.runFn3 thunk Util.refEq f a

thunk2 ∷ ∀ a b f i. Fn.Fn3 (a → b → f i) a b (Thunk f i)
thunk2 =
  let
    comp = Fn.mkFn2 \a b →
      Fn.runFn2 Util.refEq a._1 b._1 && Fn.runFn2 Util.refEq a._2 b._2
  in Fn.mkFn3 \f a b →
    Fn.runFn3 thunk comp (\{ _1, _2 } → f _1 _2) { _1: a, _2: b }

thunk3 ∷ ∀ a b c f i. Fn.Fn4 (a → b → c → f i) a b c (Thunk f i)
thunk3 =
  let
    comp = Fn.mkFn2 \a b →
      Fn.runFn2 Util.refEq a._1 b._1 && Fn.runFn2 Util.refEq a._2 b._2 && Fn.runFn2 Util.refEq a._3 b._3
  in Fn.mkFn4 \f a b c →
    Fn.runFn3 thunk comp (\{ _1, _2, _3 } → f _1 _2 _3) { _1: a, _2: b, _3: c }

runThunk ∷ ∀ f i. Thunk f i → f i
runThunk = unThunk \(Thunk g _ r a1) → g (r a1)

unsafeEqThunk ∷ ∀ f i. Fn.Fn2 (Thunk f i) (Thunk f i) Boolean
unsafeEqThunk = unThunk2 (Fn.mkFn2 \(Thunk _ c r arg) (Thunk _ c' r' arg') →
  Fn.runFn2 Util.refEq c c' && Fn.runFn2 Util.refEq r r' && Fn.runFn2 c' arg arg')

buildThunk
  ∷ ∀ eff f i a w spec node
  . (spec eff a w → V.Machine (Eff eff) (V.VDom a w) node)
  → (f i → V.VDom a w)
  → spec eff a w
  → V.Machine (Eff eff) (Thunk f i) node
buildThunk buildVDom toVDom spec = render
  where
    render t = do
      res@V.Step n _ h ← buildVDom spec (toVDom (runThunk t))
      pure (V.Step n (Fn.runFn2 patch res t) h)

    patch = Fn.mkFn2 \prev@(V.Step n step h) t → \t' → do
      if Fn.runFn2 unsafeEqThunk t t'
        then pure (V.Step n (Fn.runFn2 patch prev t) h)
        else do
          res@V.Step n' _ h' ← step (toVDom (runThunk t'))
          pure (V.Step n' (Fn.runFn2 patch res t') h')
