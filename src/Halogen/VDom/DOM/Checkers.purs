module Halogen.VDom.DOM.Checkers where

import Data.Tuple.Nested
import Halogen.VDom.DOM.Types
import Prelude

import Data.Array as Array
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Nullable (Nullable, toMaybe, toNullable)
import Data.String (toUpper)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Effect.Uncurried as EFn
import Foreign.Object as Object
import Halogen.VDom.Machine (Machine, Step, Step'(..), extract, halt, mkStep, step, unStep)
import Halogen.VDom.Machine as Machine
import Halogen.VDom.Types (ElemName(..), Namespace(..), VDom(..), runGraft)
import Halogen.VDom.Util
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM as DOM
import Web.DOM.Document as DOM
import Web.DOM.Element as DOM
import Web.DOM.Element as DOM.Element
import Web.DOM.HTMLCollection (length) as DOM.HTMLCollection
import Web.DOM.Node as DOM
import Web.DOM.NodeList (length) as DOM.NodeList
import Web.DOM.NodeType as DOM.NodeType
import Web.DOM.ParentNode (children) as DOM.ParentNode

--------------------------------------
-- Text

getElementNodeType :: DOM.Element -> DOM.NodeType
getElementNodeType element = unsafePartial $ DOM.nodeType (DOM.Element.toNode element)

checkElementIsNodeType :: DOM.NodeType -> DOM.Element -> Effect Unit
checkElementIsNodeType expectedNodeType element =
  let nodeType = getElementNodeType element
   in when (nodeType /= expectedNodeType) (throwException $ error $ "Expected element to be a " <> show expectedNodeType <> ", but got " <> show nodeType)

checkIsTextNode :: DOM.Element -> Effect Unit
checkIsTextNode = checkElementIsNodeType DOM.NodeType.TextNode

checkTextContentIsEqTo :: String -> DOM.Element -> Effect Unit
checkTextContentIsEqTo expectedText element = do
  textContent <- DOM.textContent (DOM.Element.toNode element)
  when (textContent /= expectedText) (throwException $ error $ "Expected element text content to equal to " <> quote expectedText <> ", but got " <> quote textContent)

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
  when (tagName /= expectedTagName) (throwException (error $ "Expected element tagName equal to " <> show expectedTagName <> ", but got " <> show tagName))

checkChildrenLengthIsEqualTo :: Int -> DOM.Element -> Effect Unit
checkChildrenLengthIsEqualTo expectedLength element = do
  (elementChildren :: DOM.NodeList) <- DOM.childNodes (DOM.Element.toNode element)
  elementChildrenLength <- DOM.NodeList.length elementChildren
  when (elementChildrenLength /= expectedLength) do
    EFn.runEffectFn2 warnAny "Error at " { element, elementChildren }
    (throwException (error $ "Expected element children count equal to " <> show expectedLength <> ", but got " <> show elementChildrenLength))
