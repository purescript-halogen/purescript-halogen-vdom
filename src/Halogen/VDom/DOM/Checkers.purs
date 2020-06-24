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
import Web.DOM.NodeType as DOM.NodeType
import Web.DOM.Text as DOM.Text
import Web.DOM.CharacterData as DOM.CharacterData

-- | Text checkers

checkIsTextNode :: DOM.Node -> Effect DOM.Text
checkIsTextNode node =
  case DOM.Text.fromNode node of
    Just text -> pure text
    Nothing -> do
      EFn.runEffectFn2 Util.warnAny "Error info: " { node }
      throwException $ error $ "Expected node to be a " <> show DOM.NodeType.TextNode <> ", but got " <> show (unsafePartial (DOM.Node.nodeType node)) <> " (check warning above for more information)"

checkTextContentIsEqTo :: String -> DOM.Text -> Effect Unit
checkTextContentIsEqTo expectedText text = do
  textContent <- DOM.CharacterData.data_ (DOM.Text.toCharacterData text)
  when (textContent /= expectedText) do
    EFn.runEffectFn2 Util.warnAny "Error info: " { text }
    throwException $ error $ "Expected element text content to equal to " <> Util.quote expectedText <> ", but got " <> Util.quote textContent <> " (check warning above for more information)"

-- | Elem checkers

checkIsElementNode :: DOM.Node -> Effect DOM.Element
checkIsElementNode node =
  case DOM.Element.fromNode node of
    Just text -> pure text
    Nothing -> do
      EFn.runEffectFn2 Util.warnAny "Error info: " { node }
      throwException $ error $ "Expected node to be a " <> show DOM.NodeType.ElementNode <> ", but got " <> show (unsafePartial (DOM.Node.nodeType node)) <> " (check warning above for more information)"

checkTagNameIsEqualTo :: Maybe Namespace -> ElemName -> DOM.Element -> Effect Unit
checkTagNameIsEqualTo maybeNamespace elemName element = do
  let
    -- e.g. `DIV` or `FOO:SVG`
    expectedTagName :: String
    expectedTagName = toUpper $ Util.fullAttributeName maybeNamespace elemName
  let tagName = DOM.Element.tagName element
  when (tagName /= expectedTagName) do
    EFn.runEffectFn2 Util.warnAny "Error info: " { maybeNamespace, elemName, element }
    throwException $ error $ "Expected element tagName equal to " <> show expectedTagName <> ", but got " <> show tagName <> " (check warning above for more information)"
