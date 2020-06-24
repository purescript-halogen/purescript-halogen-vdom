module Halogen.VDom.DOM.Keyed where

import Prelude

import Data.Array as Array
import Data.Function.Uncurried as Fn
import Data.List (List(..), (:))
import Data.List as List
import Data.Maybe (Maybe)
import Data.Nullable (toNullable)
import Data.Tuple (Tuple(..), fst, snd)
import Effect.Uncurried as EFn
import Foreign.Object as Object
import Halogen.VDom.DOM.Checkers (checkIsElementNode, checkTagNameIsEqualTo)
import Halogen.VDom.DOM.Types (VDomBuilder4, VDomHydrator4, VDomMachine, VDomSpec(..), VDomSpecWithHydration(..), VDomStep)
import Halogen.VDom.DOM.Util as DOMUtil
import Halogen.VDom.Machine (Step, Step'(..), extract, halt, mkStep, step)
import Halogen.VDom.Machine as Machine
import Halogen.VDom.Types (ElemName, Namespace, VDom(..), runGraft)
import Halogen.VDom.Util (warnAny)
import Halogen.VDom.Util as Util
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element) as DOM
import Web.DOM.Element as DOM.Element
import Web.DOM.Node (Node, childNodes) as DOM
import Web.DOM.NodeList as DOM.NodeList

type KeyedState a w =
  { build ∷ VDomMachine a w
  , node ∷ DOM.Node
  , attrs ∷ Step a Unit
  , ns ∷ Maybe Namespace
  , name ∷ ElemName
  , children ∷ Object.Object (VDomStep a w)
  , length ∷ Int
  }

hydrateKeyed
  ∷ ∀ a w
  . VDomHydrator4
    (Maybe Namespace)
    ElemName
    a
    (Array (Tuple String (VDom a w)))
    a
    w
hydrateKeyed = EFn.mkEffectFn8 \currentNode (VDomSpecWithHydration spec) hydrate build ns1 name1 as1 keyedChildren1 → do
  EFn.runEffectFn2 warnAny "hydrateKeyed" { ns1, name1, as1, keyedChildren1 }
  currentElement <- checkIsElementNode currentNode
  checkTagNameIsEqualTo ns1 name1 currentElement

  (currentElementChildren :: List DOM.Node) <- DOM.childNodes currentNode >>= DOM.NodeList.toArray <#> List.fromFoldable

  let (currentElementChildren' :: List DOMUtil.ElementOrTextNode) = DOMUtil.listToElementOrTextNode currentElementChildren

  (zippedChildren :: List { node :: DOM.Node, vdom :: VDom a w, key :: String }) <-
    EFn.runEffectFn6
    DOMUtil.zipChildrenAndSplitTextNodes
    (\(node :: DOMUtil.ElementOrTextNode) (Tuple key vdom) -> { node: DOMUtil.elementOrTextNodeToNode node, vdom, key })
    snd
    (case spec.vdomSpec of VDomSpec vdomSpec -> vdomSpec).document
    currentNode
    currentElementChildren'
    (List.fromFoldable keyedChildren1)

  let
    onChild :: EFn.EffectFn3 String Int ({ node :: DOM.Node, vdom :: VDom a w, key :: String }) (Step (VDom a w) DOM.Node)
    onChild = EFn.mkEffectFn3 \k ix ({ node, vdom }) → EFn.runEffectFn1 (hydrate node) vdom
  (children :: Object.Object (Step (VDom a w) DOM.Node)) ←
    EFn.runEffectFn3
    Util.strMapWithIxE
    (Array.fromFoldable zippedChildren)
    (\{ key } → key)
    onChild
  (attrs :: Step a Unit) ← EFn.runEffectFn1 (spec.hydrateAttributes currentElement) as1
  let
    state =
      { build
      , node: currentNode
      , attrs
      , ns: ns1
      , name: name1
      , children
      , length: Array.length keyedChildren1
      }
  pure $ mkStep $ Step currentNode state patchKeyed haltKeyed

buildKeyed ∷ ∀ a w. VDomBuilder4 (Maybe Namespace) ElemName a (Array (Tuple String (VDom a w))) a w
buildKeyed = EFn.mkEffectFn6 \(VDomSpec spec) build ns1 name1 as1 keyedChildren1 → do
  EFn.runEffectFn2 warnAny "buildKeyed" { ns1, name1, as1, keyedChildren1 }
  el ← EFn.runEffectFn3 Util.createElement (toNullable ns1) name1 spec.document
  let
    node :: DOM.Node
    node = DOM.Element.toNode el

    onChild :: EFn.EffectFn3 String Int (Tuple String (VDom a w)) (Step (VDom a w) DOM.Node)
    onChild = EFn.mkEffectFn3 \k ix (Tuple _ vdom) → do
      res ← EFn.runEffectFn1 build vdom
      EFn.runEffectFn3 Util.insertChildIx ix (extract res) node
      pure res
  (children :: Object.Object (Step (VDom a w) DOM.Node)) ← EFn.runEffectFn3 Util.strMapWithIxE keyedChildren1 fst onChild -- build keyed childrens
  (attrs :: Step a Unit) ← EFn.runEffectFn1 (spec.buildAttributes el) as1
  let
    state =
      { build
      , node
      , attrs
      , ns: ns1
      , name: name1
      , children
      , length: Array.length keyedChildren1
      }
  pure $ mkStep $ Step node state patchKeyed haltKeyed

patchKeyed ∷ ∀ a w. EFn.EffectFn2 (KeyedState a w) (VDom a w) (VDomStep a w)
patchKeyed = EFn.mkEffectFn2 \state vdom → do
  EFn.runEffectFn2 warnAny "patchKeyed" { state, vdom }
  let { build, node, attrs, ns: ns1, name: name1, children: keyedChildren1, length: len1 } = state
  case vdom of
    Grafted g →
      EFn.runEffectFn2 patchKeyed state (runGraft g)
    Keyed ns2 name2 as2 ch2 | Fn.runFn4 Util.eqElemSpec ns1 name1 ns2 name2 →
      case len1, Array.length ch2 of
        0, 0 → do
          attrs2 ← EFn.runEffectFn2 Machine.step attrs as2
          let
            nextState =
              { build
              , node
              , attrs: attrs2
              , ns: ns2
              , name: name2
              , children: keyedChildren1
              , length: 0
              }
          pure $ mkStep $ Step node nextState patchKeyed haltKeyed
        _, len2 → do
          let
            onThese :: EFn.EffectFn4 String Int (Step (VDom a w) DOM.Node) (Tuple String (VDom a w)) (Step (VDom a w) DOM.Node)
            onThese = EFn.mkEffectFn4 \_ ix' s (Tuple _ v) → do
              res ← EFn.runEffectFn2 step s v
              EFn.runEffectFn3 Util.insertChildIx ix' (extract res) node
              pure res

            onThis :: EFn.EffectFn2 String (Step (VDom a w) DOM.Node) Unit
            onThis = EFn.mkEffectFn2 \_ s → EFn.runEffectFn1 halt s

            onThat :: EFn.EffectFn3 String Int (Tuple String (VDom a w)) (Step (VDom a w) DOM.Node)
            onThat = EFn.mkEffectFn3 \_ ix (Tuple _ v) → do
              res ← EFn.runEffectFn1 build v
              EFn.runEffectFn3 Util.insertChildIx ix (extract res) node
              pure res
          children2 ← EFn.runEffectFn6 Util.diffWithKeyAndIxE keyedChildren1 ch2 fst onThese onThis onThat
          attrs2 ← EFn.runEffectFn2 step attrs as2
          let
            nextState =
              { build
              , node
              , attrs: attrs2
              , ns: ns2
              , name: name2
              , children: children2
              , length: len2
              }
          pure $ mkStep $ Step node nextState patchKeyed haltKeyed
    _ → do
      EFn.runEffectFn1 haltKeyed state
      EFn.runEffectFn1 build vdom

haltKeyed ∷ ∀ a w. EFn.EffectFn1 (KeyedState a w) Unit
haltKeyed = EFn.mkEffectFn1 \{ node, attrs, children } → do
  EFn.runEffectFn2 warnAny "haltKeyed" { node, attrs, children }
  parent ← EFn.runEffectFn1 Util.parentNode node
  EFn.runEffectFn2 Util.removeChild node parent
  EFn.runEffectFn2 Util.forInE children (EFn.mkEffectFn2 \_ s → EFn.runEffectFn1 halt s)
  EFn.runEffectFn1 halt attrs
