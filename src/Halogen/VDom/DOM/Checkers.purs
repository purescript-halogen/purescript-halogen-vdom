module Halogen.VDom.DOM.Checkers where

import Prelude (Unit, bind, discard, show, when, ($), (/=), (<>))

import Data.Maybe (Maybe)
import Data.String (toUpper)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Effect.Uncurried as EFn
import Halogen.VDom.Types (ElemName, Namespace)
import Halogen.VDom.Util (fullAttributeName, quote, warnAny)
import Partial.Unsafe (unsafePartial)
import Web.DOM (NodeList, NodeType) as DOM
import Web.DOM.Element (Element, tagName) as DOM
import Web.DOM.Element as DOM.Element
import Web.DOM.Node (childNodes, nodeType, textContent) as DOM
import Web.DOM.NodeList (length) as DOM.NodeList
import Web.DOM.NodeType as DOM.NodeType

--------------------------------------
-- Text


checkElementIsNodeType :: DOM.NodeType -> DOM.Element -> Effect Unit
checkElementIsNodeType = checkElementIsNodeType'
  where
    getElementNodeType :: DOM.Element -> DOM.NodeType
    getElementNodeType element = unsafePartial $ DOM.nodeType (DOM.Element.toNode element)

    checkElementIsNodeType' :: DOM.NodeType -> DOM.Element -> Effect Unit
    checkElementIsNodeType' expectedNodeType element = do
      let nodeType = getElementNodeType element
      EFn.runEffectFn2 warnAny "checkElementIsNodeType" { nodeType, expectedNodeType, meta: { element } }
      when (nodeType /= expectedNodeType) (do
        EFn.runEffectFn2 warnAny "Error at " { element }
        throwException $ error $ "Expected element to be a " <> show expectedNodeType <> ", but got " <> show nodeType)

checkIsTextNode :: DOM.Element -> Effect Unit
checkIsTextNode = checkElementIsNodeType DOM.NodeType.TextNode

checkTextContentIsEqTo :: String -> DOM.Element -> Effect Unit
checkTextContentIsEqTo expectedText element = do
  textContent <- DOM.textContent (DOM.Element.toNode element)
  EFn.runEffectFn2 warnAny "checkTextContentIsEqTo" { textContent, expectedText, meta: { element } }
  when (textContent /= expectedText) (do
    throwException $ error $ "Expected element text content to equal to " <> quote expectedText <> ", but got " <> quote textContent)

--------------------------------------
-- Elem

checkIsElementNode :: DOM.Element -> Effect Unit
checkIsElementNode = checkElementIsNodeType DOM.NodeType.ElementNode

checkTagNameIsEqualTo :: Maybe Namespace -> ElemName -> DOM.Element -> Effect Unit
checkTagNameIsEqualTo maybeNamespace elemName element = do
  let
    -- e.g. `DIV` or `FOO:SVG`
    expectedTagName :: String
    expectedTagName = toUpper $ fullAttributeName maybeNamespace elemName
  let tagName = DOM.tagName element
  EFn.runEffectFn2 warnAny "checkTagNameIsEqualTo" { expectedTagName, tagName, meta: { maybeNamespace, elemName, element } }
  when (tagName /= expectedTagName) (do
    throwException (error $ "Expected element tagName equal to " <> show expectedTagName <> ", but got " <> show tagName))

checkChildrenLengthIsEqualTo :: Int -> DOM.Element -> Effect Unit
checkChildrenLengthIsEqualTo expectedLength element = do
  (elementChildren :: DOM.NodeList) <- DOM.childNodes (DOM.Element.toNode element)
  elementChildrenLength <- DOM.NodeList.length elementChildren
  EFn.runEffectFn2 warnAny "checkChildrenLengthIsEqualTo" { elementChildrenLength, expectedLength, meta: { element, elementChildren } }
  when (elementChildrenLength /= expectedLength) do
    (throwException (error $ "Expected element children count equal to " <> show expectedLength <> ", but got " <> show elementChildrenLength))
