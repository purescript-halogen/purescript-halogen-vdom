module Test.TestVdom where

import Prelude

import Data.Bifunctor (bimap)
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, un)
import Data.Tuple (Tuple)
import Halogen.VDom as V
import Halogen.VDom.DOM.Prop (Prop(..), propFromString, buildProp, hydrateProp)
import Halogen.VDom.Thunk (Thunk, thunk1, buildThunk, hydrateThunk)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (Document) as DOM
import Web.DOM.Element (Element) as DOM
import Halogen.VDom.Machine (Machine)
import Effect (Effect)

infixr 1 prop as :=

prop ∷ ∀ a. String → String → Prop a
prop key val = Property key (propFromString val)

newtype VDom a = VDom (V.VDom (Array (Prop a)) (Thunk VDom a))

instance functorHtml ∷ Functor VDom where
  map f (VDom vdom) = VDom (bimap (map (map f)) (map f) vdom)

derive instance newtypeVDom ∷ Newtype (VDom a) _

elem ∷ ∀ a. String → Array (Prop a) → Array (VDom a) → VDom a
elem n a c = VDom $ V.Elem Nothing (V.ElemName n) a (unsafeCoerce c)

keyed ∷ ∀ a. String → Array (Prop a) → Array (Tuple String (VDom a)) → VDom a
keyed n a c = VDom $ V.Keyed Nothing (V.ElemName n) a (unsafeCoerce c)

text ∷ ∀ a. String → VDom a
text a = VDom $ V.Text a

thunk ∷ ∀ a b. (a → VDom b) → a → VDom b
thunk render val = VDom $ V.Widget $ Fn.runFn2 thunk1 render val

myfn :: ((Void → Effect Unit) -> DOM.Element -> Machine (Array (Prop Void)) Unit) → DOM.Element → Machine (Array (Prop Void)) Unit
myfn buildProp element = buildProp (const (pure unit)) element

mkSpec
  ∷ DOM.Document
  → V.VDomSpec (Array (Prop Void)) (Thunk VDom Void)
mkSpec document = V.VDomSpec
  { buildWidget: buildThunk (un VDom)
  , buildAttributes: buildProp (const (pure unit))
  , document
  }

mkSpecWithHydration
  ∷ DOM.Document
  → V.VDomSpecWithHydration (Array (Prop Void)) (Thunk VDom Void)
mkSpecWithHydration document = V.VDomSpecWithHydration
  { vdomSpec: mkSpec document
  , hydrateWidget: hydrateThunk (un VDom)
  , hydrateAttributes: hydrateProp (const (pure unit))
  }
