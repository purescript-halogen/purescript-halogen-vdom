module Halogen.VDom.Finders where

import Prelude
import Effect (Effect)
import Web.DOM.Element (Element)
import Web.DOM.ParentNode (ParentNode)
import Web.DOM.ParentNode (firstElementChild, childElementCount) as DOM.ParentNode
import Web.DOM.ParentNode (querySelector, QuerySelector(..)) as DOM
import Data.Maybe (maybe)
import Effect.Exception (error, throwException)
import Web.DOM.Element (toParentNode) as DOM.Element

findRequiredElement ∷ String → ParentNode → Effect Element
findRequiredElement selector parentNode =
  DOM.querySelector (DOM.QuerySelector selector) parentNode
    >>= maybe (throwException $ error $ selector <> " not found") pure

-- | Used for hydration
findRootElementInsideOfRootContainer :: Element -> Effect Element
findRootElementInsideOfRootContainer container = do
  childrenCount <- DOM.ParentNode.childElementCount container'
  unless (childrenCount == 1) (throwException $ error $ "Root container should have 1 child element (aka root element; it can be Element, Keyed, Text, etc.), but actual children count is " <> show childrenCount)
  rootElement ← DOM.ParentNode.firstElementChild container'
  maybe (throwException $ error $ "Root element not found") pure rootElement
  where
  container' = DOM.Element.toParentNode container
