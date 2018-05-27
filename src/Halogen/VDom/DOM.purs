module Halogen.VDom.DOM
  ( VDomSpec(..)
  , buildVDom
  , buildText
  , buildElem
  , buildKeyed
  , buildWidget
  ) where

import Prelude

import Data.Array as Array
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Tuple (Tuple(..), fst)
import Effect (foreachE)
import Effect.Uncurried as EFn
import Halogen.VDom.Machine (Step(..), Machine)
import Halogen.VDom.Machine as Machine
import Halogen.VDom.Types (ElemName(..), ElemSpec(..), Namespace(..), VDom(..), runGraft)
import Halogen.VDom.Util as Util
import Web.DOM.Document (Document) as DOM
import Web.DOM.Element (Element) as DOM
import Web.DOM.Element as DOMElement
import Web.DOM.Node (Node) as DOM

type VDomMachine a w = Machine (VDom a w) DOM.Node

type VDomStep a w = Step (VDom a w) DOM.Node

type VDomInit i a w = EFn.EffectFn1 i (VDomStep a w)

type VDomBuilder i a w = EFn.EffectFn3 (VDomSpec a w) (VDomMachine a w) i (VDomStep a w)

type VDomBuilder2 i j a w = EFn.EffectFn4 (VDomSpec a w) (VDomMachine a w) i j (VDomStep a w)

-- | Widget machines recursively reference the configured spec to potentially
-- | enable recursive trees of Widgets.
newtype VDomSpec a w = VDomSpec
  { buildWidget ∷ VDomSpec a w → Machine w DOM.Node
  , buildAttributes ∷ DOM.Element → Machine a Unit
  , document ∷ DOM.Document
  }

-- | Starts an initial `VDom` machine by providing a `VDomSpec`.
-- |
-- | ```purescript
-- | main = do
-- |   machine1 ← buildVDom spec vdomTree1
-- |   machine2 ← Machine.step machine1 vdomTree2
-- |   machine3 ← Machine.step machine2 vdomTree3
-- |   ...
-- | ````
buildVDom ∷ ∀ a w. VDomSpec a w → VDomMachine a w
buildVDom spec = build
  where
  build = EFn.mkEffectFn1 case _ of
    Text s → EFn.runEffectFn3 buildText spec build s
    Elem es ch → EFn.runEffectFn4 buildElem spec build es ch
    Keyed es ch → EFn.runEffectFn4 buildKeyed spec build es ch
    Widget w → EFn.runEffectFn3 buildWidget spec build w
    Grafted g → EFn.runEffectFn1 build (runGraft g)

buildText ∷ ∀ a w. VDomBuilder String a w
buildText = render
  where
  render = EFn.mkEffectFn3 \(VDomSpec spec) build s → do
    node ← EFn.runEffectFn2 Util.createTextNode s spec.document
    let halt = done node
    pure (Step node (Fn.runFn4 patch build halt node s) halt)

  patch = Fn.mkFn4 \build halt node s1 →
    EFn.mkEffectFn1 case _ of
      Grafted g →
        EFn.runEffectFn1 (Fn.runFn4 patch build halt node s1) (runGraft g)
      Text s2 → do
        let res = Step node (Fn.runFn4 patch build halt node s2) halt
        if s1 == s2
          then pure res
          else do
            EFn.runEffectFn2 Util.setTextContent s2 node
            pure  res
      vdom → do
        halt
        EFn.runEffectFn1 build vdom

  done node = do
    parent ← EFn.runEffectFn1 Util.parentNode node
    EFn.runEffectFn2 Util.removeChild node parent

buildElem ∷ ∀ a w. VDomBuilder2 (ElemSpec a) (Array (VDom a w)) a w
buildElem = render
  where
  render = EFn.mkEffectFn4 \(VDomSpec spec) build es1@(ElemSpec ns1 name1 as1) ch1 → do
    el ← EFn.runEffectFn3 Util.createElement (toNullable ns1) name1 spec.document
    let
      node = DOMElement.toNode el
      onChild = EFn.mkEffectFn2 \ix child → do
        res@Step n m h ← EFn.runEffectFn1 build child
        EFn.runEffectFn3 Util.insertChildIx ix n node
        pure res
    steps ← EFn.runEffectFn2 Util.forE ch1 onChild
    attrs ← EFn.runEffectFn1 (spec.buildAttributes el) as1
    let halt = Fn.runFn3 done node attrs steps
    pure (Step node (Fn.runFn6 patch build halt node attrs es1 steps) halt)

  patch = Fn.mkFn6 \build halt node attrs (es1@(ElemSpec ns1 name1 as1)) ch1 →
    EFn.mkEffectFn1 case _ of
      Grafted g →
        EFn.runEffectFn1 (Fn.runFn6 patch build halt node attrs es1 ch1) (runGraft g)
      Elem es2@(ElemSpec ns2 name2 as2) ch2 | Fn.runFn2 eqElemSpec es1 es2 → do
        case Array.length ch1, Array.length ch2 of
          0, 0 → do
            attrs' ← EFn.runEffectFn1 (Machine.step attrs) as2
            let halt' = Fn.runFn3 done node attrs' ch1
            pure (Step node (Fn.runFn6 patch build halt' node attrs' es2 ch1) halt')
          _, _ → do
            let
              onThese = EFn.mkEffectFn3 \ix prev@(Step _ step _) vdom → do
                res@(Step n' _ _) ← EFn.runEffectFn1 step vdom
                EFn.runEffectFn3 Util.insertChildIx ix n' node
                pure res
              onThis = EFn.mkEffectFn2 \ix (Step _ _ h) → h
              onThat = EFn.mkEffectFn2 \ix vdom → do
                res@(Step n _ _) ← EFn.runEffectFn1 build vdom
                EFn.runEffectFn3 Util.insertChildIx ix n node
                pure res
            steps ← EFn.runEffectFn5 Util.diffWithIxE ch1 ch2 onThese onThis onThat
            attrs' ← EFn.runEffectFn1 (Machine.step attrs) as2
            let halt' = Fn.runFn3 done node attrs' steps
            pure (Step node (Fn.runFn6 patch build halt' node attrs' es2 steps) halt')
      vdom → do
        halt
        EFn.runEffectFn1 build vdom

  done = Fn.mkFn3 \node attrs steps → do
    parent ← EFn.runEffectFn1 Util.parentNode node
    EFn.runEffectFn2 Util.removeChild node parent
    foreachE steps Machine.halt
    Machine.halt attrs

buildKeyed ∷ ∀ a w. VDomBuilder2 (ElemSpec a) (Array (Tuple String (VDom a w))) a w
buildKeyed = render
  where
  render = EFn.mkEffectFn4 \(VDomSpec spec) build es1@(ElemSpec ns1 name1 as1) ch1 → do
    el ← EFn.runEffectFn3 Util.createElement (toNullable ns1) name1 spec.document
    let
      node = DOMElement.toNode el
      onChild = EFn.mkEffectFn3 \k ix (Tuple _ vdom) → do
        res@(Step n _ _) ← EFn.runEffectFn1 build vdom
        EFn.runEffectFn3 Util.insertChildIx ix n node
        pure res
    steps ← EFn.runEffectFn3 Util.strMapWithIxE ch1 fst onChild
    attrs ← EFn.runEffectFn1 (spec.buildAttributes el) as1
    let halt = Fn.runFn3 done node attrs steps
    pure (Step node (Fn.runFn7 patch build halt node attrs es1 steps (Array.length ch1)) halt)

  patch = Fn.mkFn7 \build halt node attrs (es1@(ElemSpec ns1 name1 as1)) ch1 len1 →
    EFn.mkEffectFn1 case _ of
      Grafted g →
        EFn.runEffectFn1 (Fn.runFn7 patch build halt node attrs es1 ch1 len1) (runGraft g)
      Keyed es2@(ElemSpec ns2 name2 as2) ch2 | Fn.runFn2 eqElemSpec es1 es2 →
        case len1, Array.length ch2 of
          0, 0 → do
            attrs' ← EFn.runEffectFn1 (Machine.step attrs) as2
            let halt' = Fn.runFn3 done node attrs' ch1
            pure (Step node (Fn.runFn7 patch build halt' node attrs' es2 ch1 0) halt')
          _, len2 → do
            let
              onThese = EFn.mkEffectFn4 \_ ix' (Step _ step _) (Tuple _ vdom) → do
                res@(Step n' _ _) ← EFn.runEffectFn1 step vdom
                EFn.runEffectFn3 Util.insertChildIx ix' n' node
                pure res
              onThis = EFn.mkEffectFn2 \_ (Step _ _ h) → h
              onThat = EFn.mkEffectFn3 \_ ix (Tuple _ vdom) → do
                res@(Step n' _ _) ← EFn.runEffectFn1 build vdom
                EFn.runEffectFn3 Util.insertChildIx ix n' node
                pure res
            steps ← EFn.runEffectFn6 Util.diffWithKeyAndIxE ch1 ch2 fst onThese onThis onThat
            attrs' ← EFn.runEffectFn1 (Machine.step attrs) as2
            let halt' = Fn.runFn3 done node attrs' steps
            pure (Step node (Fn.runFn7 patch build halt' node attrs' es2 steps len2) halt')
      vdom → do
        halt
        EFn.runEffectFn1 build vdom

  done = Fn.mkFn3 \node attrs steps → do
    parent ← EFn.runEffectFn1 Util.parentNode node
    EFn.runEffectFn2 Util.removeChild node parent
    EFn.runEffectFn2 Util.forInE steps (EFn.mkEffectFn2 \_ (Step _ _ halt) → halt)
    Machine.halt attrs

buildWidget ∷ ∀ a w. VDomBuilder w a w
buildWidget = render
  where
  render = EFn.mkEffectFn3 \(VDomSpec spec) build w → do
    res@(Step n _ h) ← EFn.runEffectFn1 (spec.buildWidget (VDomSpec spec)) w
    pure (Step n (Fn.runFn2 patch build res) h)

  patch = Fn.mkFn2 \build prev@(Step node step halt) →
    EFn.mkEffectFn1 case _ of
      Grafted g →
        EFn.runEffectFn1 (Fn.runFn2 patch build prev) (runGraft g)
      Widget w → do
        res@(Step n _ h) ← EFn.runEffectFn1 step w
        pure (Step n (Fn.runFn2 patch build res) h)
      vdom → do
        halt
        EFn.runEffectFn1 build vdom

eqElemSpec ∷ ∀ a. Fn.Fn2 (ElemSpec a) (ElemSpec a) Boolean
eqElemSpec = Fn.mkFn2 \a b →
  case a, b of
    ElemSpec ns1 (ElemName name1) _, ElemSpec ns2 (ElemName name2) _ | name1 == name2 →
      case ns1, ns2 of
        Just (Namespace ns1'), Just (Namespace ns2') | ns1' == ns2' → true
        Nothing, Nothing → true
        _, _ → false
    _, _ → false
