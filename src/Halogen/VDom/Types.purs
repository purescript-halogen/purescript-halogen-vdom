module Halogen.VDom.Types
  ( VDom(..)
  , Graft
  , GraftX(..)
  , graft
  , unGraft
  , runGraft
  , ElemSpec(..)
  , Namespace(..)
  , unNamespace
  ) where

import Prelude
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Maybe (Maybe)
import Data.Tuple(Tuple)
import Unsafe.Coerce (unsafeCoerce)

data VDom a w
  = Text String
  | Elem (ElemSpec a) (Array (VDom a w))
  | Keyed (ElemSpec a) (Array (Tuple String (VDom a w)))
  | Widget w
  | Grafted (Graft a w)

instance functorVDom ∷ Functor (VDom a) where
  map g (Text a) = Text a
  map g (Grafted a) = Grafted (map g a)
  map g a = Grafted (graft (Graft id g a))

instance bifunctorVDom ∷ Bifunctor VDom where
  bimap f g (Text a) = Text a
  bimap f g (Grafted a) = Grafted (bimap f g a)
  bimap f g a = Grafted (graft (Graft f g a))

foreign import data Graft ∷ * → * → *

instance functorGraft ∷ Functor (Graft a) where
  map g = unGraft \(Graft f' g' a) → graft (Graft f' (g <<< g') a)

instance bifunctorGraft ∷ Bifunctor Graft where
  bimap f g = unGraft \(Graft f' g' a) → graft (Graft (f <<< f') (g <<< g') a)

data GraftX a a' w w' =
  Graft (a → a') (w → w') (VDom a w)

graft
  ∷ ∀ a a' w w'
  . GraftX a a' w w'
  → Graft a' w'
graft = unsafeCoerce

unGraft
  ∷ ∀ a' w' r
  . (∀ a w. GraftX a a' w w' → r)
  → Graft a' w'
  → r
unGraft f = f <<< unsafeCoerce

runGraft
  ∷ ∀ a' w'
  . Graft a' w'
  → VDom a' w'
runGraft =
  unGraft \(Graft fa fw v) →
    let
      go (Text s) = Text s
      go (Elem spec ch) = Elem (map fa spec) (map go ch)
      go (Keyed spec ch) = Keyed (map fa spec) (map (map go) ch)
      go (Widget w) = Widget (fw w)
      go (Grafted g) = Grafted (bimap fa fw g)
    in
      go v

data ElemSpec a = ElemSpec (Maybe Namespace) String a

instance functorElemSpec ∷ Functor ElemSpec where
  map f (ElemSpec ns name a) = ElemSpec ns name (f a)

newtype Namespace = Namespace String

derive instance eqNamespace ∷ Eq Namespace

unNamespace ∷ Namespace → String
unNamespace (Namespace s) = s
