module Halogen.VDom.DOM
  ( VDomEff
  , VDomMachine
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

import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst, snd)

import DOM (DOM)
import DOM.Node.Types (Element, Node, Document, elementToNode) as DOM

import Halogen.VDom.Machine (Step(..), Machine)
import Halogen.VDom.Machine as Machine
import Halogen.VDom.Types (VDom(..), ElemSpec(..), ElemName, Namespace(..), runGraft)
import Halogen.VDom.Util (forE, forInE, whenE, diffWithIxE, diffWithKeyAndIxE, strMapWithIxE, refEq)

data Quaple a b c d = Quaple a b c d

type VDomEff eff = Eff (dom ∷ DOM | eff)

type VDomMachine eff a b = Machine (VDomEff eff) a b

type VDomStep eff a b = VDomEff eff (Step (VDomEff eff) a b)

newtype VDomSpec eff a w = VDomSpec
  { buildWidget ∷ VDomSpec eff a w → VDomMachine eff w DOM.Node
  , buildAttributes ∷ DOM.Element → VDomMachine eff a Unit
  , document ∷ DOM.Document
  }

buildVDom
  ∷ ∀ eff a w
  . VDomSpec eff a w
  → VDomMachine eff (VDom a w) DOM.Node
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
  . VDomSpec eff a w
  → String
  → VDomStep eff (VDom a w) DOM.Node
buildText (VDomSpec spec) = render
  where
  render s = do
    node ← Fn.runFn2 createTextNode s spec.document
    pure (Step node (Fn.runFn2 patch node s) done)

  patch = Fn.mkFn2 \node s1 → case _ of
    Grafted g →
      Fn.runFn2 patch node s1 (runGraft g)
    Text s2 → do
      Fn.runFn2 whenE (s1 /= s2) do
        Fn.runFn2 setTextContent s2 node
      pure (Step node (Fn.runFn2 patch node s2) done)
    vdom →
      buildVDom (VDomSpec spec) vdom

  done = pure unit

buildElem
  ∷ ∀ eff a w
  . VDomSpec eff a w
  → ElemSpec a
  → Array (VDom a w)
  → VDomStep eff (VDom a w) DOM.Node
buildElem (VDomSpec spec) = render
  where
  render es1@(ElemSpec ns1 name1 as1) ch1 = do
    el ← Fn.runFn3 createElem ns1 name1 spec.document
    attrs ← spec.buildAttributes el as1
    let
      node = DOM.elementToNode el
      onChild = Fn.mkFn2 \ix child → do
        Step n m h ← buildVDom (VDomSpec spec) child
        Fn.runFn2 appendChild n node
        pure (Tuple m h)
    steps ← Fn.runFn2 forE ch1 onChild
    pure
      (Step node
        (Fn.runFn4 patch node attrs es1 steps)
        (Fn.runFn2 done attrs steps))

  patch = Fn.mkFn4 \node attrs (es1@(ElemSpec ns1 name1 as1)) ch1 → case _ of
    Grafted g →
      Fn.runFn4 patch node attrs es1 ch1 (runGraft g)
    Elem es2@(ElemSpec ns2 name2 as2) ch2 | Fn.runFn2 eqElemSpec es1 es2 → do
      attrs' ← Machine.step attrs as2
      let
        onThese = Fn.mkFn3 \ix (Tuple step _) vdom → do
          Step n' m' h' ← step vdom
          n ← Fn.runFn2 unsafeChildIx ix node
          Fn.runFn2 whenE (not (Fn.runFn2 refEq n' n)) do
            Fn.runFn3 replaceChild n' n node
          pure (Tuple m' h')
        onThis = Fn.mkFn2 \ix (Tuple _ halt) → do
          halt
          removeLastChild node
        onThat = Fn.mkFn2 \ix vdom → do
          Step n m h ← buildVDom (VDomSpec spec) vdom
          Fn.runFn2 appendChild n node
          pure (Tuple m h)
      steps ← Fn.runFn5 diffWithIxE ch1 ch2 onThese onThis onThat
      pure
        (Step node
          (Fn.runFn4 patch node attrs' es2 steps)
          (Fn.runFn2 done attrs' steps))
    vdom →
      buildVDom (VDomSpec spec) vdom

  done = Fn.mkFn2 \attrs steps → do
    foreachE steps snd
    Machine.halt attrs

buildKeyed
  ∷ ∀ eff a w
  . VDomSpec eff a w
  → ElemSpec a
  → Array (Tuple String (VDom a w))
  → VDomStep eff (VDom a w) DOM.Node
buildKeyed (VDomSpec spec) = render
  where
  render es1@(ElemSpec ns1 name1 as1) ch1 = do
    el ← Fn.runFn3 createElem ns1 name1 spec.document
    attrs ← spec.buildAttributes el as1
    let
      node = DOM.elementToNode el
      onChild = Fn.mkFn3 \k ix (Tuple _ vdom) → do
        Step n m h ← buildVDom (VDomSpec spec) vdom
        Fn.runFn2 appendChild n node
        pure (Quaple k ix m h)
    steps ← Fn.runFn3 strMapWithIxE ch1 fst onChild
    pure
      (Step node
        (Fn.runFn4 patch node attrs es1 steps)
        (Fn.runFn2 done attrs steps))

  patch = Fn.mkFn4 \node attrs (es1@(ElemSpec ns1 name1 as1)) ch1 → case _ of
    Grafted g →
      Fn.runFn4 patch node attrs es1 ch1 (runGraft g)
    Keyed es2@(ElemSpec ns2 name2 as2) ch2 | Fn.runFn2 eqElemSpec es1 es2 → do
      attrs' ← Machine.step attrs as2
      let
        onThese = Fn.mkFn4 \k ix (Quaple _ ix' step _) (Tuple _ vdom) →
          if ix == ix'
            then do
              Step n' m' h' ← step vdom
              n ← Fn.runFn2 unsafeChildIx ix node
              Fn.runFn2 whenE (not (Fn.runFn2 refEq n' n))
                (Fn.runFn3 replaceChild n' n node)
              pure (Quaple k ix m' h')
            else do
              Step n' m' h' ← step vdom
              Fn.runFn3 unsafeInsertChildIx ix n' node
              pure (Quaple k ix m' h')
        onThis = Fn.mkFn2 \k (Quaple _ _ _ halt) → do
          halt
          removeLastChild node
        onThat = Fn.mkFn3 \k ix (Tuple _ vdom) → do
          Step n' m' h' ← buildVDom (VDomSpec spec) vdom
          Fn.runFn3 unsafeInsertChildIx ix n' node
          pure (Quaple k ix m' h')
      steps ← Fn.runFn6 diffWithKeyAndIxE ch1 ch2 fst onThese onThis onThat
      pure
        (Step node
          (Fn.runFn4 patch node attrs' es2 steps)
          (Fn.runFn2 done attrs' steps))
    vdom →
      buildVDom (VDomSpec spec) vdom

  done = Fn.mkFn2 \attrs steps → do
    Fn.runFn2 forInE steps (Fn.mkFn2 \_ (Quaple _ _ _ halt) → halt)
    Machine.halt attrs

buildWidget
  ∷ ∀ eff a w
  . VDomSpec eff a w
  → w
  → VDomStep eff (VDom a w) DOM.Node
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

foreign import replaceChild
  ∷ ∀ eff
  . Fn.Fn3 DOM.Node DOM.Node DOM.Node (Eff (dom ∷ DOM | eff) Unit)

foreign import removeLastChild
  ∷ ∀ eff
  . DOM.Node → (Eff (dom ∷ DOM | eff) Unit)

foreign import appendChild
  ∷ ∀ eff
  . Fn.Fn2 DOM.Node DOM.Node (Eff (dom ∷ DOM | eff) Unit)

foreign import unsafeInsertChildIx
  ∷ ∀ eff
  . Fn.Fn3 Int DOM.Node DOM.Node (Eff (dom ∷ DOM | eff) Unit)

foreign import unsafeChildIx
  ∷ ∀ eff
  . Fn.Fn2 Int DOM.Node (Eff (dom ∷ DOM | eff) DOM.Node)
