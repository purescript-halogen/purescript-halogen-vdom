module Halogen.VDom.DOM
  ( VDomMachine
  , VDomStep
  , VDomSpec(..)
  , buildVDom
  , buildText
  , buildElem
  , buildKeyed
  , buildWidget
  , createElem
  ) where

import Prelude
import Control.Monad.Eff (Eff, foreachE)

import Data.Array as Array
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)

import DOM (DOM)
import DOM.Node.Types (Element, Node, Document, elementToNode) as DOM

import Halogen.VDom.Machine (Step(..), Machine)
import Halogen.VDom.Machine as Machine
import Halogen.VDom.Types (VDom(..), ElemSpec(..), ElemName, Namespace(..), runGraft)
import Halogen.VDom.Util (forE, forInE, replicateE, diffWithIxE, diffWithKeyAndIxE, strMapWithIxE, refEq)

type VDomMachine eff a b = Machine (Eff eff) a b

type VDomStep eff a b = Eff eff (Step (Eff eff) a b)

newtype VDomSpec eff a w = VDomSpec
  { buildWidget ∷ VDomSpec eff a w → VDomMachine eff w DOM.Node
  , buildAttributes ∷ DOM.Element → VDomMachine eff a Unit
  , document ∷ DOM.Document
  }

type VDomEffects eff = (dom ∷ DOM | eff)

buildVDom
  ∷ ∀ eff a w
  . VDomSpec (VDomEffects eff) a w
  → VDomMachine (VDomEffects eff) (VDom a w) DOM.Node
buildVDom spec = render
  where
  render = case _ of
    Text s → buildText spec s
    Elem es ch → buildElem spec es ch
    Keyed es ch → buildKeyed spec es ch
    Widget w → buildWidget spec w
    Grafted g → buildVDom spec (runGraft g)

buildText
  ∷ ∀ eff a w
  . VDomSpec (VDomEffects eff) a w
  → String
  → VDomStep (VDomEffects eff) (VDom a w) DOM.Node
buildText (VDomSpec spec) = render
  where
  render s = do
    node ← Fn.runFn2 createTextNode s spec.document
    pure (Step node (Fn.runFn2 patch node s) done)

  patch = Fn.mkFn2 \node s1 → case _ of
    Grafted g →
      Fn.runFn2 patch node s1 (runGraft g)
    Text s2 → do
      let
        res = Step node (Fn.runFn2 patch node s2) done
      case s1 == s2 of
        true → effPure res
        _ → do
          Fn.runFn2 setTextContent s2 node
          pure res
    vdom →
      buildVDom (VDomSpec spec) vdom

  done = pure unit

buildElem
  ∷ ∀ eff a w
  . VDomSpec (VDomEffects eff) a w
  → ElemSpec a
  → Array (VDom a w)
  → VDomStep (VDomEffects eff) (VDom a w) DOM.Node
buildElem (VDomSpec spec) = render
  where
  render es1@(ElemSpec ns1 name1 as1) ch1 = do
    el ← Fn.runFn3 createElem ns1 name1 spec.document
    let
      node = DOM.elementToNode el
      onChild = Fn.mkFn2 \ix child → do
        res@Step n m h ← buildVDom (VDomSpec spec) child
        Fn.runFn3 insertChildIx ix n node
        pure res
    steps ← Fn.runFn2 forE ch1 onChild
    attrs ← spec.buildAttributes el as1
    pure
      (Step node
        (Fn.runFn5 patch node attrs es1 steps (Array.length ch1))
        (Fn.runFn2 done attrs steps))

  patch = Fn.mkFn5 \node attrs (es1@(ElemSpec ns1 name1 as1)) ch1 len1 → case _ of
    Grafted g →
      Fn.runFn5 patch node attrs es1 ch1 len1 (runGraft g)
    Elem es2@(ElemSpec ns2 name2 as2) ch2 | Fn.runFn2 eqElemSpec es1 es2 →
      case len1, Array.length ch2 of
        0, 0 → do
          attrs' ← Machine.step attrs as2
          pure
            (Step node
              (Fn.runFn5 patch node attrs' es2 ch1 0)
              (Fn.runFn2 done attrs' ch1))
        _, len2 → do
          let
            onThese = Fn.mkFn3 \ix (Step n step halt) vdom → do
              res@Step n' m' h' ← step vdom
              Fn.runFn3 insertChildIx ix n' node
              case Fn.runFn2 refEq n' n of
                true → pure res
                _ → do
                  halt
                  pure res
            onThis = Fn.mkFn2 \ix (Step _ _ halt) → do
              halt
            onThat = Fn.mkFn2 \ix vdom → do
              res@Step n m h ← buildVDom (VDomSpec spec) vdom
              Fn.runFn3 insertChildIx ix n node
              pure res
          steps ← Fn.runFn5 diffWithIxE ch1 ch2 onThese onThis onThat
          lenD ← childNodesLength node
          Fn.runFn2 replicateE (lenD - len2) (removeLastChild node)
          attrs' ← Machine.step attrs as2
          pure
            (Step node
              (Fn.runFn5 patch node attrs' es2 steps len2)
              (Fn.runFn2 done attrs' steps))
    vdom →
      buildVDom (VDomSpec spec) vdom

  done = Fn.mkFn2 \attrs steps → do
    foreachE steps Machine.halt
    Machine.halt attrs

buildKeyed
  ∷ ∀ eff a w
  . VDomSpec (VDomEffects eff) a w
  → ElemSpec a
  → Array (Tuple String (VDom a w))
  → VDomStep (VDomEffects eff) (VDom a w) DOM.Node
buildKeyed (VDomSpec spec) = render
  where
  render es1@(ElemSpec ns1 name1 as1) ch1 = do
    el ← Fn.runFn3 createElem ns1 name1 spec.document
    let
      node = DOM.elementToNode el
      onChild = Fn.mkFn3 \k ix (Tuple _ vdom) → do
        res@Step n m h ← buildVDom (VDomSpec spec) vdom
        Fn.runFn3 insertChildIx ix n node
        pure res
    steps ← Fn.runFn3 strMapWithIxE ch1 fst onChild
    attrs ← spec.buildAttributes el as1
    pure
      (Step node
        (Fn.runFn5 patch node attrs es1 steps (Array.length ch1))
        (Fn.runFn2 done attrs steps))

  patch = Fn.mkFn5 \node attrs (es1@(ElemSpec ns1 name1 as1)) ch1 len1 → case _ of
    Grafted g →
      Fn.runFn5 patch node attrs es1 ch1 len1 (runGraft g)
    Keyed es2@(ElemSpec ns2 name2 as2) ch2 | Fn.runFn2 eqElemSpec es1 es2 →
      case len1, Array.length ch2 of
        0, 0 → do
          attrs' ← Machine.step attrs as2
          pure
            (Step node
              (Fn.runFn5 patch node attrs' es2 ch1 0)
              (Fn.runFn2 done attrs' ch1))
        _, len2 → do
          let
            onThese = Fn.mkFn4 \k ix' (Step n step halt) (Tuple _ vdom) → do
              res@Step n' m' h' ← step vdom
              Fn.runFn3 insertChildIx ix' n' node
              case Fn.runFn2 refEq n' n of
                true → pure res
                _ → do
                  halt
                  pure res
            onThis = Fn.mkFn2 \k (Step _ _ halt) → do
              halt
            onThat = Fn.mkFn3 \k ix (Tuple _ vdom) → do
              res@Step n' m' h' ← buildVDom (VDomSpec spec) vdom
              Fn.runFn3 insertChildIx ix n' node
              pure res
          steps ← Fn.runFn6 diffWithKeyAndIxE ch1 ch2 fst onThese onThis onThat
          lenD ← childNodesLength node
          Fn.runFn2 replicateE (lenD - len2) (removeLastChild node)
          attrs' ← Machine.step attrs as2
          pure
            (Step node
              (Fn.runFn5 patch node attrs' es2 steps len2)
              (Fn.runFn2 done attrs' steps))
    vdom →
      buildVDom (VDomSpec spec) vdom

  done = Fn.mkFn2 \attrs steps → do
    Fn.runFn2 forInE steps (Fn.mkFn2 \_ (Step _ _ halt) → halt)
    Machine.halt attrs

buildWidget
  ∷ ∀ eff a w
  . VDomSpec (VDomEffects eff) a w
  → w
  → VDomStep (VDomEffects eff) (VDom a w) DOM.Node
buildWidget (VDomSpec spec) = render
  where
  render w = do
    Step n m h ← spec.buildWidget (VDomSpec spec) w
    pure (Step n (patch m) h)

  patch step = case _ of
    Grafted g →
      patch step (runGraft g)
    Widget w → do
      Step n m h ← step w
      pure (Step n (patch m) h)
    vdom →
      buildVDom (VDomSpec spec) vdom

createElem
  ∷ ∀ eff
  . Fn.Fn3 (Maybe Namespace) ElemName DOM.Document (Eff (dom ∷ DOM | eff) DOM.Element)
createElem = Fn.mkFn3 \ns name doc →
  case ns of
    Nothing → Fn.runFn2 createElement name doc
    Just n  → Fn.runFn3 createElementNS n name doc

eqElemSpec
  ∷ ∀ a
  . Fn.Fn2 (ElemSpec a) (ElemSpec a) Boolean
eqElemSpec = Fn.mkFn2 \a b →
  case a, b of
    ElemSpec ns1 name1 _, ElemSpec ns2 name2 _ | name1 == name2 →
      case ns1, ns2 of
        Just (Namespace ns1'), Just (Namespace ns2') | ns1' == ns2' → true
        Nothing, Nothing → true
        _, _ → false
    _, _ → false

effPure ∷ ∀ eff a. a → Eff eff a
effPure = pure

foreign import createTextNode
  ∷ ∀ eff
  . Fn.Fn2 String DOM.Document (Eff (dom ∷ DOM | eff) DOM.Node)

foreign import setTextContent
  ∷ ∀ eff
  . Fn.Fn2 String DOM.Node (Eff (dom ∷ DOM | eff) Unit)

foreign import createElement
  ∷ ∀ eff
  . Fn.Fn2 ElemName DOM.Document (Eff (dom ∷ DOM | eff) DOM.Element)

foreign import createElementNS
  ∷ ∀ eff
  . Fn.Fn3 Namespace ElemName DOM.Document (Eff (dom ∷ DOM | eff) DOM.Element)

foreign import removeLastChild
  ∷ ∀ eff
  . DOM.Node → (Eff (dom ∷ DOM | eff) Unit)

foreign import insertChildIx
  ∷ ∀ eff
  . Fn.Fn3 Int DOM.Node DOM.Node (Eff (dom ∷ DOM | eff) Unit)

foreign import unsafeChildIx
  ∷ ∀ eff
  . Fn.Fn2 Int DOM.Node (Eff (dom ∷ DOM | eff) DOM.Node)

foreign import childNodesLength
  ∷ ∀ eff
  . DOM.Node → (Eff (dom ∷ DOM | eff) Int)
