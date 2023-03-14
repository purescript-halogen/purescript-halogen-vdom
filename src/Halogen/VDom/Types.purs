module Halogen.VDom.Types
  ( VDom(..)
  , Graft
  , GraftX(..)
  , graft
  , unGraft
  , runGraft
  , ElemName(..)
  , Namespace(..)
  , FnObject(..)
  , ShimmerHolder(..)
  , ShimmerItem(..)
  ) where

import Prelude
import Effect (Effect)
import Effect.Uncurried as EFn
import Data.Bifunctor (class Bifunctor, bimap)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple)
import Unsafe.Coerce (unsafeCoerce)

-- | The core virtual-dom tree type, where `a` is the type of attributes,
-- | and `w` is the type of "widgets". Widgets are machines that have complete
-- | control over the lifecycle of some `DOM.Node`.
-- |
-- | The `Grafted` constructor and associated machinery enables `bimap`
-- | fusion using a Coyoneda-like encoding.
data VDom a w
  = Text String
  | Elem (Maybe Namespace) ElemName a (Array (VDom a w))
  | Chunk (Maybe Namespace) ElemName a (ShimmerHolder a w)
  | Keyed (Maybe Namespace) ElemName a (Array (Tuple String (VDom a w)))
  | Widget w
  | Grafted (Graft a w)
  | Microapp String a (Maybe (Array (VDom a w)))

instance functorVDom ∷ Functor (VDom a) where
  map _ (Text a) = Text a
  map g (Grafted a) = Grafted (map g a)
  map g a = Grafted (graft (Graft identity g a))

instance bifunctorVDom ∷ Bifunctor VDom where
  bimap _ _ (Text a) = Text a
  bimap f g (Grafted a) = Grafted (bimap f g a)
  bimap f g a = Grafted (graft (Graft f g a))

foreign import data Graft ∷ Type → Type → Type

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
      go (Elem ns n a ch) = Elem ns n (fa a) (map go ch)
      go (Keyed ns n a ch) = Keyed ns n (fa a) (map (map go) ch)
      go (Widget w) = Widget (fw w)
      go (Grafted g) = Grafted (bimap fa fw g)
      go (Microapp s a child) = Microapp s (fa a) $
        case child of
          Just ch -> (Just $ map go ch)
          _ -> Nothing
      go (Chunk ns n a ch) = Chunk ns n (fa a) (chunkMap go ch)
    in
      go v

chunkMap :: forall a a' w w'. (VDom a w -> VDom a' w') -> ShimmerHolder a w -> ShimmerHolder a' w'
chunkMap go shimHolder = map (shimmerItemMap go) shimHolder

shimmerItemMap :: forall a a' w w'. (VDom a w -> VDom a' w') -> ShimmerItem a w -> ShimmerItem a' w'
shimmerItemMap go item = { shimmer : (go item.shimmer) , actualLayout : (go item.actualLayout) }

newtype ElemName = ElemName String

derive instance newtypeElemName ∷ Newtype ElemName _
derive newtype instance eqElemName ∷ Eq ElemName
derive newtype instance ordElemName ∷ Ord ElemName

newtype Namespace = Namespace String

derive instance newtypeNamespace ∷ Newtype Namespace _
derive newtype instance eqNamespace ∷ Eq Namespace
derive newtype instance ordNamespace ∷ Ord Namespace

type ShimmerHolder a w = Array (ShimmerItem a w)

type ShimmerItem a w = 
  { shimmer :: VDom a w 
  , actualLayout :: VDom a w
  }

type FnObject =
  { replaceView :: forall a . EFn.EffectFn3 a String (Array String) Unit
  , setManualEvents :: forall a b. a -> b -> Effect Unit
  , updateChildren :: forall a. EFn.EffectFn1 a Unit
  , removeChild :: forall a b. EFn.EffectFn3 a b Int Unit
  , createPrestoElement:: forall a. Effect a
  , cancelBehavior :: EFn.EffectFn1 String Unit
  , manualEventsName :: Array String
  , updateMicroAppPayload :: ∀ b. EFn.EffectFn3 String b Boolean Unit
  , updateProperties :: forall a b. EFn.EffectFn2 a b Unit
  , parseParams :: forall a b c d. EFn.EffectFn3 a b c d
  }
