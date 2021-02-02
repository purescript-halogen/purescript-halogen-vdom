module Halogen.VDom.Thunk
  ( Thunk
  , buildThunk
  , hydrateThunk
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
import Halogen.VDom.DOM.Types (VDomMachine)

foreign import data ThunkArg ∷ Type

foreign import data ThunkId ∷ Type

data Thunk :: (Type -> Type) -> Type -> Type
data Thunk f i
  = Thunk
    ThunkId
    (Fn.Fn2 ThunkArg ThunkArg Boolean) -- (oldArg -> newArg -> isEqual)
    (ThunkArg → f i) -- (oldArg -> output)
    ThunkArg -- oldArg

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

thunked ∷ ∀ a f i. (a → a → Boolean) → (a → f i) → a → Thunk f i
thunked eqFn f =
  let
    tid = unsafeThunkId { f }
    eqFn' = Fn.mkFn2 eqFn
  in
    \a → Fn.runFn4 thunk tid eqFn' f a

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
unsafeEqThunk = Fn.mkFn2 \(Thunk id eqFn _ renderArg) (Thunk id' eqFn' _ renderArg') →
  Fn.runFn2 Util.refEq id id' &&
  Fn.runFn2 Util.refEq eqFn eqFn' &&
  Fn.runFn2 eqFn renderArg renderArg'

type ThunkState :: (Type -> Type) -> Type -> Type -> Type -> Type
type ThunkState f i a w =
  { thunk ∷ Thunk f i -- prev thunk
  , vdom ∷ M.Step (V.VDom a w) Node
  }

hydrateThunk
  ∷ ∀ f i a w
  . (f i → V.VDom a w)
  → V.VDomSpecWithHydration a w
  → Node
  → V.Machine (Thunk f i) Node
hydrateThunk toVDom spec element = mkThunkBuilder (V.hydrateVDom spec element) toVDom

buildThunk
  ∷ ∀ f i a w
  . (f i → V.VDom a w)
  → V.VDomSpec a w
  → V.Machine (Thunk f i) Node
buildThunk toVDom spec = mkThunkBuilder (V.buildVDom spec) toVDom

mkThunkBuilder
  ∷ ∀ f i a w
  . VDomMachine a w
  → (f i → V.VDom a w)
  → V.Machine (Thunk f i) Node
mkThunkBuilder build toVDom = renderThunk
  where
  renderThunk ∷ V.Machine (Thunk f i) Node
  renderThunk = EFn.mkEffectFn1 \t → do
    vdom ← EFn.runEffectFn1 build (toVDom (runThunk t))
    pure $ M.mkStep $ M.Step (M.extract vdom) { thunk: t, vdom } patchThunk haltThunk

  patchThunk ∷ EFn.EffectFn2 (ThunkState f i a w) (Thunk f i) (V.Step (Thunk f i) Node)
  patchThunk = EFn.mkEffectFn2 \state t2 → do
    let { vdom: prev, thunk: t1 } = state
    if Fn.runFn2 unsafeEqThunk t1 t2 -- if eq
      then pure $ M.mkStep $ M.Step (M.extract prev) state patchThunk haltThunk -- dont run effect
      else do
        vdom ← EFn.runEffectFn2 M.step prev (toVDom (runThunk t2)) -- else create new vdom, execute step (compare and patch if need)
        pure $ M.mkStep $ M.Step (M.extract vdom) { vdom, thunk: t2 } patchThunk haltThunk

  haltThunk ∷ EFn.EffectFn1 (ThunkState f i a w) Unit
  haltThunk = EFn.mkEffectFn1 \state → do
    EFn.runEffectFn1 M.halt state.vdom
