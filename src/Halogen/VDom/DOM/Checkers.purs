module Halogen.VDom.DOM.Checkers where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (error, throwException)
import Effect.Uncurried as EFn
import Halogen.VDom.Types (ElemName(..), Namespace)
import Halogen.VDom.Util as Util
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM as DOM
import Web.DOM.CharacterData as DOM.CharacterData
import Web.DOM.Element as DOM.Element
import Web.DOM.Node as DOM.Node
import Web.DOM.NodeType as DOM.NodeType
import Web.DOM.Text as DOM.Text

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
checkTagNameIsEqualTo maybeNamespace (ElemName elemName) element = do
  let
    localName :: String
    localName = DOM.Element.localName element

  when (localName /= elemName) do
    EFn.runEffectFn2 Util.warnAny "Error info: " { maybeNamespace, elemName, element }
    throwException $ error $ "Expected element localName equal to " <> show elemName <> ", but got " <> show localName <> " (check warning above for more information)"

  let
    maybeNamespace' :: Maybe String
    maybeNamespace' = unsafeCoerce maybeNamespace

    namespaceURI :: Maybe String
    namespaceURI = DOM.Element.namespaceURI element

  when (namespaceURI /= maybeNamespace') do
    EFn.runEffectFn2 Util.warnAny "Error info: " { maybeNamespace, elemName, element }
    throwException $ error $ "Expected element namespaceURI equal to " <> show maybeNamespace' <> ", but got " <> show namespaceURI <> " (check warning above for more information)"
