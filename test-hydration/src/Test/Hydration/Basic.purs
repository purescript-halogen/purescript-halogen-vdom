module Test.Hydration.Basic where

import Prelude

import Data.Bifunctor (bimap)
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Tuple (Tuple)
import Halogen.VDom (VDomHydrationSpec(..), VDomSpec(..)) as V
import Halogen.VDom.DOM.Prop (Prop(..), buildProp, hydrateProp, propFromString)
import Halogen.VDom.Thunk (Thunk, buildThunk, hydrateThunk, thunk1)
import Halogen.VDom.Types (ElemName(..))
import Halogen.VDom.Types (VDom(..)) as V
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM as DOM

attr ∷ ∀ a. String → String → Prop a
attr key value = Attribute Nothing key value

prop ∷ ∀ a. String → String → Prop a
prop key value = Property key (propFromString value)

infixr 1 attr as :=
infixr 1 prop as .=

type Attribute a = Array (Prop a)

type Widget a = Thunk VDom a

type Core a = V.VDom (Attribute a) (Widget a)

newtype VDom a = VDom (Core a)

instance Functor VDom where
  map f (VDom vdom) = VDom (bimap (map (map f)) (map f) vdom)

derive instance Newtype (VDom a) _

elem ∷ ∀ a. String → Array (Prop a) → Array (VDom a) → VDom a
elem n a c = VDom $ V.Elem Nothing (ElemName n) a (unsafeCoerce c)

keyed ∷ ∀ a. String → Array (Prop a) → Array (Tuple String (VDom a)) → VDom a
keyed n a c = VDom $ V.Keyed Nothing (ElemName n) a (unsafeCoerce c)

text ∷ ∀ a. String → VDom a
text a = VDom $ V.Text a

thunk ∷ ∀ a b. (a → VDom b) → a → VDom b
thunk render val = VDom $ V.Widget $ Fn.runFn2 thunk1 render val

type VDomSpec a = V.VDomSpec (Attribute a) (Widget a)

mkSpec ∷ ∀ a. DOM.Document → VDomSpec a
mkSpec document = V.VDomSpec
  { buildWidget: buildThunk (un VDom)
  , buildAttributes: buildProp (const (pure unit))
  , document
  }

type VDomHydrationSpec a = V.VDomHydrationSpec (Attribute a) (Widget a)

mkSpecWithHydration ∷ ∀ a. DOM.Document → VDomHydrationSpec a
mkSpecWithHydration document = V.VDomHydrationSpec
  { vdomSpec: mkSpec document
  , hydrateWidget: hydrateThunk (un VDom)
  , hydrateAttributes: hydrateProp (const (pure unit))
  }
