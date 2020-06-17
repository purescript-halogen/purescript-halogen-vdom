module Halogen.VDom.DOM.Elem where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Halogen.VDom.DOM.Types (VDomBuilder4, VDomHydrator4, VDomMachine, VDomSpec(..), VDomStep)
import Halogen.VDom.DOM.Checkers (checkChildrenLengthIsEqualTo, checkIsElementNode, checkTagNameIsEqualTo)
import Data.Array (length, zip, fromFoldable) as Array
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Effect.Uncurried as EFn
import Effect (Effect)
import Halogen.VDom.Machine (Step, Step'(..), extract, halt, mkStep, step)
import Halogen.VDom.Types (ElemName, Namespace, VDom(..), runGraft)
import Halogen.VDom.Util as Util
import Halogen.VDom.DOM.Util as DOMUtil
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM as DOM
import Web.DOM.Element as DOM.Element
import Web.DOM.Node as DOM.Node
import Web.DOM.Text as DOM.Text
import Web.DOM.NodeList as DOM.NodeList
import Web.DOM.Document as DOM.Document
import Web.DOM.CharacterData as DOM.CharacterData
import Data.List (List(..), (:))
import Data.List as List
import Data.String as String
import Effect.Exception (error, throwException)
import Data.Traversable (for)
import Control.Alt ((<|>))

type ElemState a w =
  { build ∷ VDomMachine a w
  , node ∷ DOM.Node
  , attrs ∷ Step a Unit
  , ns ∷ Maybe Namespace
  , name ∷ ElemName
  , children ∷ Array (VDomStep a w)
  }

data ElementOrTextNode = ElementNode DOM.Element | TextNode DOM.Text

elementOrTextNodeToNode :: ElementOrTextNode -> DOM.Node
elementOrTextNodeToNode referenceNode =
  case referenceNode of
    ElementNode element -> DOM.Element.toNode element
    TextNode text -> DOM.Text.toNode text

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
zipChildrenAndSplitTextNodes :: forall a w . VDomSpec a w -> DOM.Node -> List ElementOrTextNode -> List (VDom a w) -> Effect (List { node :: ElementOrTextNode, vdom :: VDom a w })
zipChildrenAndSplitTextNodes (VDomSpec spec) parent domChildren ((Text "") : vdomChildrenTail) = do
  -- | when DOM is `<div>foo</div>` and vdom is `HH.div_ [HH.text "foo", HH.text ""]` - it wont touch the "foo", but should append new text node "" after "foo"
  -- | when DOM is `<div></div>` (no children) and vdom is `HH.div_ [HH.text ""]` - it will create append new text node
  EFn.runEffectFn2 Util.warnAny "zipChildrenAndSplitTextNodes 1" { parent, domChildren, vdomChildrenTail }

  (newChildWithEmptyText :: DOM.Text) <- DOM.Document.createTextNode "" spec.document

  case domChildren of
    Nil -> void $ DOM.Node.appendChild (DOM.Text.toNode newChildWithEmptyText) parent
    (referenceNode : _) -> do
      let (referenceNode' :: DOM.Node) = elementOrTextNodeToNode referenceNode
      void $ DOM.Node.insertBefore (DOM.Text.toNode newChildWithEmptyText) referenceNode' parent

  tailResult <- zipChildrenAndSplitTextNodes (VDomSpec spec) parent domChildren vdomChildrenTail
  pure ({ node: TextNode newChildWithEmptyText, vdom: Text "" } : tailResult)
zipChildrenAndSplitTextNodes (VDomSpec spec) parent (TextNode textNode : domChildrenTail) ((Text expectedText) : vdomChildrenTail) = do
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
      tailResult <- zipChildrenAndSplitTextNodes (VDomSpec spec) parent domChildrenTail vdomChildrenTail
      pure ({ node: TextNode textNode, vdom: Text expectedText } : tailResult)

    -- | when DOM is `<div>foobar</div>` and vdom is `HH.div_ [HH.text "foo", HH.text "bar"]` - it should split "foobar" on "foo" and "bar"
    GT -> do
      EFn.runEffectFn2 Util.warnAny "zipChildrenAndSplitTextNodes 2 GT" { }
      nextTextNode <- DOM.Text.splitText expectedTextLength textNode -- this is our "bar", and textNode is now our "foo" (but was - "foobar")
      tailResult <- zipChildrenAndSplitTextNodes (VDomSpec spec) parent (TextNode nextTextNode : domChildrenTail) vdomChildrenTail
      pure ({ node: TextNode textNode, vdom: Text expectedText } : tailResult)
zipChildrenAndSplitTextNodes spec parent (domChild : domChildrenTail) (vdomChild : vdomChildrenTail) = do
  EFn.runEffectFn2 Util.warnAny "zipChildrenAndSplitTextNodes 3" {}
  tailResult <- zipChildrenAndSplitTextNodes spec parent domChildrenTail vdomChildrenTail
  pure ({ node: domChild, vdom: vdomChild } : tailResult)
zipChildrenAndSplitTextNodes spec parent Nil Nil = pure Nil
zipChildrenAndSplitTextNodes spec parent otherDomChildren otherVdomChildren = throwException $ error $ "[zipChildrenAndSplitTextNodes] unexpected input"

toElementOrTextNode :: DOM.Node -> Maybe ElementOrTextNode
toElementOrTextNode node = (DOM.Text.fromNode node <#> TextNode) <|> (DOM.Element.fromNode node <#> ElementNode)

listToElementOrTextNode :: List DOM.Node -> List ElementOrTextNode
listToElementOrTextNode = map toElementOrTextNode >>> List.catMaybes

hydrateElem
  ∷ ∀ a w
  . VDomHydrator4
    (Maybe Namespace)
    ElemName
    a
    (Array (VDom a w))
    a
    w
hydrateElem = EFn.mkEffectFn8 \currentNode (VDomSpec spec) hydrate build ns1 name1 as1 ch1 → do
  -- | let
  -- |   normalizedChildren = DOMUtil.normalizeChildren ch1

  currentElement <- checkIsElementNode currentNode
  checkTagNameIsEqualTo ns1 name1 currentElement
  -- | checkChildrenLengthIsEqualTo (Array.length normalizedChildren) currentElement

  (currentElementChildren :: List DOM.Node) <- DOM.Node.childNodes currentNode >>= DOM.NodeList.toArray <#> List.fromFoldable

  let (currentElementChildren' :: List ElementOrTextNode) = listToElementOrTextNode currentElementChildren

  (zippedChildren :: List { node :: ElementOrTextNode, vdom :: VDom a w }) <- zipChildrenAndSplitTextNodes (VDomSpec spec) currentNode currentElementChildren' (List.fromFoldable ch1)

  let
    onChild :: { node :: ElementOrTextNode, vdom :: VDom a w } -> Effect (Step (VDom a w) DOM.Node)
    onChild { node, vdom } = EFn.runEffectFn1 (hydrate (elementOrTextNodeToNode node)) vdom
  (children :: Array (Step (VDom a w) DOM.Node)) <- for zippedChildren onChild <#> Array.fromFoldable
  (attrs :: Step a Unit) ← EFn.runEffectFn1 (spec.hydrateAttributes currentElement) as1
  let
    state =
      { build
      , node: currentNode
      , attrs
      , ns: ns1
      , name: name1
      , children
      }
  pure $ mkStep $ Step currentNode state patchElem haltElem

buildElem
  ∷ ∀ a w
  . VDomBuilder4
    (Maybe Namespace)
    ElemName
    a
    (Array (VDom a w))
    a
    w
buildElem = EFn.mkEffectFn6 \(VDomSpec spec) build ns1 name1 as1 ch1 → do
  el ← EFn.runEffectFn3 Util.createElement (toNullable ns1) name1 spec.document
  let
    node :: DOM.Node
    node = DOM.Element.toNode el

    onChild :: EFn.EffectFn2 Int (VDom a w) (Step (VDom a w) DOM.Node)
    onChild = EFn.mkEffectFn2 \ix child → do
      (res :: Step (VDom a w) DOM.Node) ← EFn.runEffectFn1 build child
      EFn.runEffectFn3 Util.insertChildIx ix (extract res) node
      pure res
  children ← EFn.runEffectFn2 Util.forE ch1 onChild
  attrs ← EFn.runEffectFn1 (spec.buildAttributes el) as1
  let
    state =
      { build
      , node
      , attrs
      , ns: ns1
      , name: name1
      , children
      }
  pure $ mkStep $ Step node state patchElem haltElem

patchElem ∷ ∀ a w. EFn.EffectFn2 (ElemState a w) (VDom a w) (VDomStep a w)
patchElem = EFn.mkEffectFn2 \state vdom → do
  let { build, node, attrs, ns: ns1, name: name1, children: ch1 } = state
  case vdom of
    Grafted g →
      EFn.runEffectFn2 patchElem state (runGraft g)
    Elem ns2 name2 as2 ch2 | Fn.runFn4 Util.eqElemSpec ns1 name1 ns2 name2 → do -- if new vdom is elem AND new and old are equal
      case Array.length ch1, Array.length ch2 of
        0, 0 → do
          attrs2 ← EFn.runEffectFn2 step attrs as2
          let
            nextState =
              { build
              , node
              , attrs: attrs2
              , ns: ns2
              , name: name2
              , children: ch1
              }
          pure $ mkStep $ Step node nextState patchElem haltElem
        _, _ → do
          let
            -- both elements are found
            onThese :: EFn.EffectFn3 Int (Step (VDom a w) DOM.Node) (VDom a w) (Step (VDom a w) DOM.Node)
            onThese = EFn.mkEffectFn3 \ix (ch1Elem :: VDomStep a w) (ch2Elem :: VDom a w) → do
              -- execute step function (compare previous dom and ch2Elem), the patchXXX function will be called for ch2Elem element
              -- if elements are different - old element is removed from DOM, replaced with new but not yet attached to DOM
              res ← EFn.runEffectFn2 step ch1Elem ch2Elem
              EFn.runEffectFn3 Util.insertChildIx ix (extract res) node
              pure res

            -- there are no more new elements in the new list, but there is an element in old list
            onThis :: EFn.EffectFn2 Int (Step (VDom a w) DOM.Node) Unit
            onThis = EFn.mkEffectFn2 \ix ch1Elem → EFn.runEffectFn1 halt ch1Elem

            -- there are no more new elements in the old list, but there is an element in new list
            onThat :: EFn.EffectFn2 Int (VDom a w) (Step (VDom a w) DOM.Node)
            onThat = EFn.mkEffectFn2 \ix ch2Elem → do
              res ← EFn.runEffectFn1 build ch2Elem
              EFn.runEffectFn3 Util.insertChildIx ix (extract res) node
              pure res
          (children2 :: Array (Step (VDom a w) DOM.Node)) ← EFn.runEffectFn5 Util.diffWithIxE ch1 ch2 onThese onThis onThat
          (attrs2 :: Step a Unit) ← EFn.runEffectFn2 step attrs as2
          let
            nextState =
              { build
              , node
              , attrs: attrs2
              , ns: ns2
              , name: name2
              , children: children2
              }
          pure $ mkStep $ Step node nextState patchElem haltElem
    _ → do
      EFn.runEffectFn1 haltElem state
      EFn.runEffectFn1 build vdom

haltElem ∷ ∀ a w. EFn.EffectFn1 (ElemState a w) Unit
haltElem = EFn.mkEffectFn1 \{ node, attrs, children } → do
  parent ← EFn.runEffectFn1 Util.parentNode node
  EFn.runEffectFn2 Util.removeChild node parent
  EFn.runEffectFn2 Util.forEachE children halt
  EFn.runEffectFn1 halt attrs
