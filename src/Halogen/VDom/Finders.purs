module Halogen.VDom.Finders where

import Prelude

import Data.Either (Either(..), note)
import Effect (Effect)
import Web.DOM (Element)
import Web.DOM.Element (toParentNode) as DOM.Element
import Web.DOM.ParentNode (firstElementChild, childElementCount) as DOM.ParentNode

-- | Used for hydration in halogen code and in halogen-vdom tests
findElementFirstChild :: Element -> Effect (Either String Element)
findElementFirstChild container = do
  let
    container' = DOM.Element.toParentNode container

  childrenCount <- DOM.ParentNode.childElementCount container'

  if childrenCount /= 1
    then pure $ Left $ "Root container should have only 1 child element (aka root element; it can be Element, Keyed, Text, etc.), but actual children count is " <> show childrenCount
    else do
      maybeRootElement <- DOM.ParentNode.firstElementChild container'
      pure $ note "Root element not found" $ maybeRootElement
