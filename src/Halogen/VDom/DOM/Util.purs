module Halogen.VDom.DOM.Util where

import Prelude

import Halogen.VDom.Types (VDom(..))
import Data.List as List
import Data.List (List(..), (:))
import Web.DOM as DOM
import Web.DOM.Element as DOM.Element
import Web.DOM.Node as DOM.Node
import Web.DOM.Text as DOM.Text
import Web.DOM.Document as DOM.Document
import Web.DOM.CharacterData as DOM.CharacterData
import Data.Maybe (Maybe)
import Control.Alt ((<|>))
import Effect.Exception (error, throwException)
import Effect.Uncurried as EFn
import Halogen.VDom.Util as Util
import Data.String as String

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

-- | The idea is to prevent rerendering on the next render after hydration
-- | but because in the prerendered html all Text nodes are merged (i.e. `HH.div_ [ HH.text "foo", HH.text "bar" ]` is rendered as `<div>foobar</div>`, not `<div>"foo""bar"</div>`)
-- | and empty text nodes "" are hidden (i.e. they exist in $0.childNodes, but are not rendered),
-- | we need to split text nodes using .splitText() and insert "" nodes where it is needed
-- |
-- | Check https://jsbin.com/bukulicito/edit?html,output to see how text nodes are added to the parent
-- | The code was tested on this example https://github.com/srghma/purescript-halogen-nextjs/tree/master/examples/text-nodes
-- |
-- | Also, to undestand what's happening, you need to understand how textNode.splitText() works:
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
    DOM.Document
    DOM.Node
    (List ElementOrTextNode)
    (List vdomContainer)
    (List output)
zipChildrenAndSplitTextNodes = EFn.mkEffectFn6 \toOutput extractVdom document parent domChildren vdomContainerChildren ->
  case domChildren, vdomContainerChildren of
    Nil, Nil -> pure Nil
    _, (vdomContainerChild : vdomContainerChildrenTail) ->
      let vdomChild = extractVdom vdomContainerChild
      in case domChildren, vdomChild of
        -- | Expected text is "", but it wasnt rendered in dom (it is never rendered) - add the "" to the dom
        _, Text "" -> do
          (newChildWithEmptyText :: DOM.Text) <- DOM.Document.createTextNode "" document

          case domChildren of
            -- | when DOM is `<div></div>` (no children) and vdom is `HH.div_ [HH.text ""]` - it will create append new text node
            Nil -> void $ DOM.Node.appendChild (DOM.Text.toNode newChildWithEmptyText) parent
            -- | when DOM is `<div>foo</div>` and vdom is `HH.div_ [HH.text "foo", HH.text ""]` - it wont touch the "foo", but should append new text node "" after "foo"
            (referenceNode : _) -> do
              let (referenceNode' :: DOM.Node) = elementOrTextNodeToNode referenceNode
              void $ DOM.Node.insertBefore (DOM.Text.toNode newChildWithEmptyText) referenceNode' parent

          let (head :: output) = toOutput (TextNode newChildWithEmptyText) vdomContainerChild

          (tailResult :: List output) <- EFn.runEffectFn6 zipChildrenAndSplitTextNodes toOutput extractVdom document parent domChildren vdomContainerChildrenTail

          pure (head : tailResult)
        -- | We have Text in dom and Text in vdom
        -- | when DOM is `<div>foobar</div>` and vdom is `HH.div_ [HH.text "foobarbaz"]` - throw error (LT case)
        -- | when DOM is `<div>foobar</div>` and vdom is `HH.div_ [HH.text "foobar"]` - it should just hydrate the node (EQ case)
        -- | when DOM is `<div>foobar</div>` and vdom is `HH.div_ [HH.text "foo", HH.text "bar"]` - it should split "foobar" on "foo" and "bar" (GT case)
        (TextNode textNode : domChildrenTail), (Text expectedText) -> do
          textNodeLength <- DOM.CharacterData.length (DOM.Text.toCharacterData textNode)

          let expectedTextLength = String.length expectedText

          case compare textNodeLength expectedTextLength of
            LT -> do
              textNodeData <- DOM.CharacterData.data_ (DOM.Text.toCharacterData textNode)

              EFn.runEffectFn2 Util.warnAny "Error info: " { textNode, expectedText }

              throwException $ error $ "[zipChildrenAndSplitTextNodes] The Text node length should not smaller then expected. Expected length = " <> show expectedTextLength <> ", actual = " <> show textNodeLength <> " (check warning above for more information)"
            EQ -> do
              let (head :: output) = toOutput (TextNode textNode) vdomContainerChild

              tailResult <- EFn.runEffectFn6 zipChildrenAndSplitTextNodes toOutput extractVdom document parent domChildrenTail vdomContainerChildrenTail

              pure (head : tailResult)
            GT -> do
              nextTextNode <- DOM.Text.splitText expectedTextLength textNode -- this is our "bar", and textNode is now our "foo" (but was - "foobar")

              let (head :: output) = toOutput (TextNode textNode) vdomContainerChild

              tailResult <- EFn.runEffectFn6 zipChildrenAndSplitTextNodes toOutput extractVdom document parent (TextNode nextTextNode : domChildrenTail) vdomContainerChildrenTail

              pure (head : tailResult)
        (domChild : domChildrenTail), _ -> do
          let (head :: output) = toOutput domChild vdomContainerChild

          tailResult <- EFn.runEffectFn6 zipChildrenAndSplitTextNodes toOutput extractVdom document parent domChildrenTail vdomContainerChildrenTail

          pure (head : tailResult)
        domChildren', vdomChild' -> do
          EFn.runEffectFn2 Util.warnAny "Error info: " { domChildren: domChildren', vdomChild: vdomChild' }
          throwException $ error $ "[zipChildrenAndSplitTextNodes] Unexpected input (check warning above for more information)"
    domChildren', vdomChildren' -> do
      EFn.runEffectFn2 Util.warnAny "Error info: " { domChildren: domChildren', vdomChildren: vdomChildren' }
      throwException $ error $ "[zipChildrenAndSplitTextNodes] Unexpected input (check warning above for more information)"
