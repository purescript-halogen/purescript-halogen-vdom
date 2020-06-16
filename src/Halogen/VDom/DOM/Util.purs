module Halogen.VDom.DOM.Util where

import Prelude

import Data.Array (fromFoldable) as Array
import Halogen.VDom.Types (VDom(..))
import Data.List as List
import Data.List (List(..), (:))

-- | e.g.
-- |   [Text ""] -> []
-- |   [Text "foo", Text "bar"] -> [Text "foobar"]
normalizeChildren :: forall a w . Array (VDom a w) -> Array (VDom a w)
normalizeChildren = Array.fromFoldable <<< List.foldr go Nil <<< List.fromFoldable
  where
    go :: VDom a w -> List (VDom a w) -> List (VDom a w)
    go (Text "") accum = accum
    go (Text text2) (Text text1 : accumt) = Text (text1 <> text2) : accumt
    go vdom accum = vdom : accum
