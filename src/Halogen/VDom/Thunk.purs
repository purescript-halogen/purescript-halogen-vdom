module Halogen.VDom.Thunk
  ( Thunk
  , buildThunk
  , runThunk
  , hoist
  , mapThunk
  , thunked
  , thunk1
  , thunk2
  , thunk3
  ) where

import Prelude

import Data.Function.Uncurried as Fn
import Effect.Uncurried as EFn
import Halogen.VDom as V
import Halogen.VDom.Machine as M
import Halogen.VDom.Util as Util
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Node (Node)

foreign import data ThunkArg ∷ Type

foreign import data ThunkId ∷ Type

data Thunk f i = Thunk ThunkId (Fn.Fn2 ThunkArg ThunkArg Boolean) (ThunkArg → f i) ThunkArg

unsafeThunkId ∷ ∀ a. a → ThunkId
unsafeThunkId = unsafeCoerce

instance functorThunk ∷ Functor f ⇒ Functor (Thunk f) where
  map f (Thunk a b c d) = Thunk a b (c >>> map f) d

hoist ∷ ∀ f g. (f ~> g) → Thunk f ~> Thunk g
hoist = mapThunk

mapThunk ∷ ∀ f g i j. (f i -> g j) → Thunk f i -> Thunk g j
mapThunk k (Thunk a b c d) = Thunk a b (c >>> k) d

thunk ∷ ∀ a f i. Fn.Fn4 ThunkId (Fn.Fn2 a a Boolean) (a → f i) a (Thunk f i)
thunk = Fn.mkFn4 \tid eqFn f a →
  Thunk tid
    (unsafeCoerce eqFn ∷ Fn.Fn2 ThunkArg ThunkArg Boolean)
    (unsafeCoerce f ∷ ThunkArg → f i)
    (unsafeCoerce a ∷ ThunkArg)

-- | Creates thunk with custom equality
-- |
-- | Also a unique reference is created, to stress the fact, that one
-- | application of rendering function (`f` argument) and equality (`eqFn` argument) is only ever equal to itself
-- |
-- | It's done because of a distinction of how compiler generalizes equality functions
-- | For more information check [HOW_THUNKED_FUNCTION_WORKS.md](https://github.com/purescript-halogen/purescript-halogen-vdom/blob/master/HOW_THUNKED_FUNCTION_WORKS.md) document

thunked ∷ ∀ a f i. (a → a → Boolean) → (a → f i) → a → Thunk f i
thunked eqFn f =
  let
    tid = unsafeThunkId { f }
    eqFn' = Fn.mkFn2 eqFn
  in
    \a → Fn.runFn4 thunk tid eqFn' f a

-- | Creates thunk with equality that is fixed to referential equality (`===`)
-- |
-- | Works faster for types like `String` or `Integer`,
-- | but will not work for arrays and objects
thunk1 ∷ ∀ a f i. Fn.Fn2 (a → f i) a (Thunk f i)
thunk1 = Fn.mkFn2 \f a → Fn.runFn4 thunk (unsafeThunkId f) Util.refEq f a

thunk2 ∷ ∀ a b f i. Fn.Fn3 (a → b → f i) a b (Thunk f i)
thunk2 =
  let
    eqFn = Fn.mkFn2 \a b →
      Fn.runFn2 Util.refEq a._1 b._1 &&
      Fn.runFn2 Util.refEq a._2 b._2
  in
    Fn.mkFn3 \f a b →
      Fn.runFn4 thunk (unsafeThunkId f) eqFn (\{ _1, _2 } → f _1 _2) { _1: a, _2: b }

thunk3 ∷ ∀ a b c f i. Fn.Fn4 (a → b → c → f i) a b c (Thunk f i)
thunk3 =
  let
    eqFn = Fn.mkFn2 \a b →
      Fn.runFn2 Util.refEq a._1 b._1 &&
      Fn.runFn2 Util.refEq a._2 b._2 &&
      Fn.runFn2 Util.refEq a._3 b._3
  in
    Fn.mkFn4 \f a b c →
      Fn.runFn4 thunk (unsafeThunkId f) eqFn (\{ _1, _2, _3 } → f _1 _2 _3) { _1: a, _2: b, _3: c }

runThunk ∷ ∀ f i. Thunk f i → f i
runThunk (Thunk _ _ render arg) = render arg

unsafeEqThunk ∷ ∀ f i. Fn.Fn2 (Thunk f i) (Thunk f i) Boolean
unsafeEqThunk = Fn.mkFn2 \(Thunk a1 b1 _ d1) (Thunk a2 b2 _ d2) →
  Fn.runFn2 Util.refEq a1 a2 &&
  Fn.runFn2 Util.refEq b1 b2 &&
  Fn.runFn2 b1 d1 d2

type ThunkState f i a w =
  { thunk ∷ Thunk f i
  , vdom ∷ M.Step (V.VDom a w) Node
  }

buildThunk
  ∷ ∀ f i a w
  . (f i → V.VDom a w)
  → V.VDomSpec a w
  → V.Machine (Thunk f i) Node
buildThunk toVDom = renderThunk
  where
  renderThunk ∷ V.VDomSpec a w → V.Machine (Thunk f i) Node
  renderThunk spec = EFn.mkEffectFn1 \t → do
    vdom ← EFn.runEffectFn1 (V.buildVDom spec) (toVDom (runThunk t))
    pure $ M.mkStep $ M.Step (M.extract vdom) { thunk: t, vdom } patchThunk haltThunk

  patchThunk ∷ EFn.EffectFn2 (ThunkState f i a w) (Thunk f i) (V.Step (Thunk f i) Node)
  patchThunk = EFn.mkEffectFn2 \state t2 → do
    let { vdom: prev, thunk: t1 } = state
    if Fn.runFn2 unsafeEqThunk t1 t2
      then pure $ M.mkStep $ M.Step (M.extract prev) state patchThunk haltThunk
      else do
        vdom ← EFn.runEffectFn2 M.step prev (toVDom (runThunk t2))
        pure $ M.mkStep $ M.Step (M.extract vdom) { vdom, thunk: t2 } patchThunk haltThunk

  haltThunk ∷ EFn.EffectFn1 (ThunkState f i a w) Unit
  haltThunk = EFn.mkEffectFn1 \state → do
    EFn.runEffectFn1 M.halt state.vdom
