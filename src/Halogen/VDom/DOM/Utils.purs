module Halogen.VDom.DOM.Utils where

import Halogen.VDom.DOM.Types
import Prelude

import Data.Array as Array
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Nullable (Nullable, toNullable)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Effect.Uncurried as EFn
import Foreign.Object as Object
import Halogen.VDom.Machine (Machine, Step, Step'(..), extract, halt, mkStep, step, unStep)
import Halogen.VDom.Machine as Machine
import Halogen.VDom.Types (ElemName(..), Namespace(..), VDom(..), runGraft)
import Halogen.VDom.Util as Util
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (Document) as DOM
import Web.DOM.Element (Element) as DOM
import Web.DOM.Element as DOMElement
import Web.DOM.Node (Node) as DOM

eqElemSpec ∷ Fn.Fn4 (Maybe Namespace) ElemName (Maybe Namespace) ElemName Boolean
eqElemSpec = Fn.mkFn4 \ns1 (ElemName name1) ns2 (ElemName name2) →
  if name1 == name2
    then case ns1, ns2 of
      Just (Namespace ns1'), Just (Namespace ns2') | ns1' == ns2' → true
      Nothing, Nothing → true
      _, _ → false
    else false

quote :: String -> String
quote s = "\"" <> s <> "\""

--------------------------------------
-- Text

checkNodeIsTextNode :: DOM.Node -> Effect Unit
checkNodeIsTextNode node =
  EFn.runEffectFn1 Util.nodeIsTextNode node >>= if _
    then pure unit
    else do
      nodeType <- EFn.runEffectFn1 Util.getNodeType node
      throwException (error $ "Expected node to be a text node (nodeType === 3), but got " <> show nodeType)

checkNodeTextContentIsEqTo :: String -> DOM.Node -> Effect Unit
checkNodeTextContentIsEqTo s node = do
  textContent <- EFn.runEffectFn1 Util.getTextContent node
  if textContent == s
    then pure unit
    else throwException (error $ "Expected node text content to equal to " <> quote s <> ", but got " <> quote textContent)

--------------------------------------
-- Elem

checkNodeIsElementNode :: DOM.Node -> Effect Unit
checkNodeIsElementNode node =
  EFn.runEffectFn1 Util.nodeIsElementNode node >>= if _
    then pure unit
    else do
      nodeType <- EFn.runEffectFn1 Util.getNodeType node
      throwException (error $ "Expected node to be element node (nodeType === 1), but got " <> show nodeType)

checkNodeNamespaceIsEqualTo :: Nullable Namespace -> DOM.Node -> Effect Unit
checkNodeNamespaceIsEqualTo namespace node = do
  nullableNamespaceURI <- EFn.runEffectFn1 Util.getNamespaceURI node
  if nullableNamespaceURI == namespace
    then pure unit
    else throwException (error $ "Expected node namespaceURI equal to " <> quote (show namespace) <> ", but got " <> quote (show nullableNamespaceURI))

checkNodeNameIsEqualTo :: ElemName -> DOM.Node -> Effect Unit
checkNodeNameIsEqualTo = undefined

checkNodeChildrenLengthIsEqualTo :: Int -> DOM.Node -> Effect Unit
checkNodeChildrenLengthIsEqualTo = undefined

undefined :: ∀ a . a
undefined = unsafeCoerce unit
