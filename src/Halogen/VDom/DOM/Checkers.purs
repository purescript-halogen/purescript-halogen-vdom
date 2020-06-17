module Halogen.VDom.DOM.Checkers where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String (toUpper)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Effect.Uncurried as EFn
import Halogen.VDom.Types (ElemName, Namespace)
import Halogen.VDom.Util as Util
import Partial.Unsafe (unsafePartial)
import Web.DOM as DOM
import Web.DOM.Element as DOM.Element
import Web.DOM.Node as DOM.Node
import Web.DOM.NodeList as DOM.NodeList
import Web.DOM.NodeType as DOM.NodeType
import Web.DOM.Text as DOM.Text
import Web.DOM.CharacterData as DOM.CharacterData

--------------------------------------
-- Text

checkIsTextNode :: DOM.Node -> Effect DOM.Text
checkIsTextNode node =
  case DOM.Text.fromNode node of
    Just text -> pure text
    Nothing -> do
      EFn.runEffectFn2 Util.warnAny "Error at " { node }
      throwException $ error $ "Expected node to be a " <> show DOM.NodeType.TextNode <> ", but got " <> show (unsafePartial (DOM.Node.nodeType node))

checkTextContentIsEqTo :: String -> DOM.Text -> Effect Unit
checkTextContentIsEqTo expectedText text = do
  textContent <- DOM.CharacterData.data_ (DOM.Text.toCharacterData text)
  EFn.runEffectFn2 Util.warnAny "checkTextContentIsEqTo" { textContent, expectedText, meta: { text } }
  when (textContent /= expectedText) (do
    throwException $ error $ "Expected element text content to equal to " <> Util.quote expectedText <> ", but got " <> Util.quote textContent)

--------------------------------------
-- Elem

checkIsElementNode :: DOM.Node -> Effect DOM.Element
checkIsElementNode node =
  case DOM.Element.fromNode node of
    Just text -> pure text
    Nothing -> do
      EFn.runEffectFn2 Util.warnAny "Error at " { node }
      throwException $ error $ "Expected node to be a " <> show DOM.NodeType.ElementNode <> ", but got " <> show (unsafePartial (DOM.Node.nodeType node))

checkTagNameIsEqualTo :: Maybe Namespace -> ElemName -> DOM.Element -> Effect Unit
checkTagNameIsEqualTo maybeNamespace elemName element = do
  let
    -- e.g. `DIV` or `FOO:SVG`
    expectedTagName :: String
    expectedTagName = toUpper $ Util.fullAttributeName maybeNamespace elemName
  let tagName = DOM.Element.tagName element
  EFn.runEffectFn2 Util.warnAny "checkTagNameIsEqualTo" { expectedTagName, tagName, meta: { maybeNamespace, elemName, element } }
  when (tagName /= expectedTagName) (do
    throwException (error $ "Expected element tagName equal to " <> show expectedTagName <> ", but got " <> show tagName))

checkChildrenLengthIsEqualTo :: Int -> DOM.Element -> Effect Unit
checkChildrenLengthIsEqualTo expectedLength element = do
  (elementChildren :: DOM.NodeList) <- DOM.Node.childNodes (DOM.Element.toNode element)
  elementChildrenLength <- DOM.NodeList.length elementChildren
  EFn.runEffectFn2 Util.warnAny "checkChildrenLengthIsEqualTo" { elementChildrenLength, expectedLength, meta: { element, elementChildren } }
  when (elementChildrenLength /= expectedLength) do
    (throwException (error $ "Expected element children count equal to " <> show expectedLength <> ", but got " <> show elementChildrenLength))
