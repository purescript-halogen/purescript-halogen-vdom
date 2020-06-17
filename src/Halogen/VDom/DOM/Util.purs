module Halogen.VDom.DOM.Util where

import Prelude

import Data.Array (fromFoldable) as Array
import Halogen.VDom.Types (VDom(..))
import Data.List as List
import Data.List (List(..), (:))
import Web.DOM as DOM
import Web.DOM.Element as DOM.Element
import Web.DOM.Node as DOM.Node
import Web.DOM.Text as DOM.Text
import Web.DOM.NodeList as DOM.NodeList
import Web.DOM.Document as DOM.Document
import Web.DOM.CharacterData as DOM.CharacterData
import Data.Maybe (Maybe(..))
import Control.Alt ((<|>))
import Halogen.VDom.DOM.Types (VDomBuilder4, VDomHydrator4, VDomMachine, VDomSpec(..), VDomStep)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Effect.Uncurried as EFn
import Halogen.VDom.Util as Util
import Data.String as String
import Unsafe.Coerce (unsafeCoerce)

data ElementOrTextNode = ElementNode DOM.Element | TextNode DOM.Text

elementOrTextNodeToNode :: ElementOrTextNode -> DOM.Node
elementOrTextNodeToNode referenceNode =
  case referenceNode of
    ElementNode element -> DOM.Element.toNode element
    TextNode text -> DOM.Text.toNode text

toElementOrTextNode :: DOM.Node -> Maybe ElementOrTextNode
toElementOrTextNode node = (DOM.Text.fromNode node <#> TextNode) <|> (DOM.Element.fromNode node <#> ElementNode)

listToElementOrTextNode :: List DOM.Node -> List ElementOrTextNode
listToElementOrTextNode = map toElementOrTextNode >>> List.catMaybes


-- | The idea is to prevent rerendering on the next render
-- | but because in the prerendered html all text nodes are merged (`HH.div_ [ HH.text "foo", HH.text "bar" ]` rendered as `<div>foobar</div>`, not `<div>"foo""bar"</div>`)
-- | and empty text nodes "" are hidden (i.e. they exist in $0.childNodes, but not rendered),
-- | we need to split text nodes using .splitText() and insert "" nodes where it is needed
-- |
-- | check https://jsbin.com/bukulicito/edit?html,output to see how text nodes are added to the parent
-- |
-- | How textNode.splitText() works:
-- | 1. when $0 is '<div>foobar</div>' -> $0.childNodes[0].splitText(0) -> returns "foobar", does nothing
-- | 2. when $0 is '<div>foobar</div>' -> $0.childNodes[0].splitText(3) -> splits on "foo" and "bar", returns "bar"
-- | 3. when $0 is '<div>foobar</div>' -> $0.childNodes[0].splitText(6) -> adds new node "" after "foobar", returns ""
-- | 4. when $0 is '<div>foobar</div>' -> $0.childNodes[0].splitText(100) -> throws "Uncaught DOMException: Failed to execute 'splitText' on 'Text': The offset 100 is larger than the Text node's length."
-- | 5. when $0 is '<div>foobar</div>' -> $0.childNodes[0].splitText(-100) -> throws "Uncaught DOMException: Failed to execute 'splitText' on 'Text': The offset 4294966996 is larger than the Text node's length."
zipChildrenAndSplitTextNodes
  :: forall a w vdomContainer output
   . EFn.EffectFn6
    (ElementOrTextNode -> vdomContainer -> output)
    (vdomContainer -> VDom a w)
    (VDomSpec a w)
    DOM.Node
    (List ElementOrTextNode)
    (List vdomContainer)
    (List output)
zipChildrenAndSplitTextNodes = EFn.mkEffectFn6 \toOutput extractVdom (VDomSpec spec) parent domChildren vdomChildren ->
  case domChildren, vdomChildren of
    _, (vdomChild : vdomChildrenTail) ->
      let vdomChild' = extractVdom vdomChild
      in case domChildren, vdomChild' of
        _, Text "" -> do
          EFn.runEffectFn2 Util.warnAny "zipChildrenAndSplitTextNodes 1" { parent, domChildren, vdomChildrenTail }

          (newChildWithEmptyText :: DOM.Text) <- DOM.Document.createTextNode "" spec.document

          case domChildren of
            -- | when DOM is `<div></div>` (no children) and vdom is `HH.div_ [HH.text ""]` - it will create append new text node
            Nil -> void $ DOM.Node.appendChild (DOM.Text.toNode newChildWithEmptyText) parent
            -- | when DOM is `<div>foo</div>` and vdom is `HH.div_ [HH.text "foo", HH.text ""]` - it wont touch the "foo", but should append new text node "" after "foo"
            (referenceNode : _) -> do
              let (referenceNode' :: DOM.Node) = elementOrTextNodeToNode referenceNode
              void $ DOM.Node.insertBefore (DOM.Text.toNode newChildWithEmptyText) referenceNode' parent

          let (head :: output) = toOutput (TextNode newChildWithEmptyText) vdomChild

          (tailResult :: List output) <- EFn.runEffectFn6 zipChildrenAndSplitTextNodes toOutput extractVdom (VDomSpec spec) parent domChildren vdomChildrenTail

          pure (head : tailResult)
        (TextNode textNode : domChildrenTail), (Text expectedText) -> do
          EFn.runEffectFn2 Util.warnAny "zipChildrenAndSplitTextNodes 2" { parent, textNode, domChildrenTail, expectedText, vdomChildrenTail }
          textNodeLength <- DOM.CharacterData.length (DOM.Text.toCharacterData textNode)

          let expectedTextLength = String.length expectedText

          case compare textNodeLength expectedTextLength of
            LT -> do
              textNodeData <- DOM.CharacterData.data_ (DOM.Text.toCharacterData textNode)
              EFn.runEffectFn2 Util.warnAny "zipChildrenAndSplitTextNodes 2 LT" { }
              throwException $ error $ "should not smaller then expected " <> textNodeData -- TODO: better errors

            -- | when DOM is `<div>foobar</div>` and vdom is `HH.div_ [HH.text "foobar"]` - it should just hydrate
            EQ -> do
              EFn.runEffectFn2 Util.warnAny "zipChildrenAndSplitTextNodes 2 EQ" { }

              let (head :: output) = toOutput (TextNode textNode) vdomChild

              tailResult <- EFn.runEffectFn6 zipChildrenAndSplitTextNodes toOutput extractVdom (VDomSpec spec) parent domChildrenTail vdomChildrenTail

              pure (head : tailResult)

            -- | when DOM is `<div>foobar</div>` and vdom is `HH.div_ [HH.text "foo", HH.text "bar"]` - it should split "foobar" on "foo" and "bar"
            GT -> do
              EFn.runEffectFn2 Util.warnAny "zipChildrenAndSplitTextNodes 2 GT" { }
              nextTextNode <- DOM.Text.splitText expectedTextLength textNode -- this is our "bar", and textNode is now our "foo" (but was - "foobar")

              let (head :: output) = toOutput (TextNode textNode) vdomChild

              tailResult <- EFn.runEffectFn6 zipChildrenAndSplitTextNodes toOutput extractVdom (VDomSpec spec) parent (TextNode nextTextNode : domChildrenTail) vdomChildrenTail

              pure (head : tailResult)
        (domChild : domChildrenTail), _ -> do
          EFn.runEffectFn2 Util.warnAny "zipChildrenAndSplitTextNodes 3" {}

          let (head :: output) = toOutput domChild vdomChild

          tailResult <- EFn.runEffectFn6 zipChildrenAndSplitTextNodes toOutput extractVdom (VDomSpec spec) parent domChildrenTail vdomChildrenTail

          pure (head : tailResult)
        _, _ -> throwException $ error $ "[zipChildrenAndSplitTextNodes] unexpected input"
    Nil, Nil -> pure Nil
    _, _ -> throwException $ error $ "[zipChildrenAndSplitTextNodes] unexpected input"
