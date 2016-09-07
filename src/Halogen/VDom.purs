module Halogen.VDom where

import Prelude
import Data.Array as Array
import Data.Bifunctor (bimap)
import Data.Function.Uncurried as Fn
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Nullable as Null
import Data.Tuple (Tuple(..), snd)
import Control.Monad.Eff (Eff, foreachE)
import Unsafe.Coerce (unsafeCoerce)

import DOM (DOM)
import DOM.Node.Document (createElement, createElementNS, createTextNode) as DOM
import DOM.Node.Types (Element, Node, Document, NodeList, elementToNode, textToNode) as DOM
import DOM.Node.Node (childNodes, firstChild, setTextContent) as DOM
import DOM.Node.NodeList (length) as DOM

import Halogen.VDom.Machine (Step(..), Machine)
import Halogen.VDom.Machine as Machine

data VDom k a w
  = Text String
  | Elem (ElemSpec a) (Array (VDom k a w))
  | Keyed (ElemSpec a) (Array (Tuple k (VDom k a w)))
  | Widget w
  | Grafted (Graft k a w)

instance functorVDom ∷ Functor (VDom k a) where
  map f (Text s) = Text s
  map f (Grafted g) = Grafted (map f g)
  map f v = Grafted (graft (Graft id id f v))

foreign import data Graft ∷ * → * → * → *

instance functorGraft ∷ Functor (Graft k a) where
  map f = unGraft \(Graft fk fa fw v) → graft (Graft fk fa (f <<< fw) v)

data GraftX k k' a a' w w' =
  Graft (k → k') (a → a') (w → w') (VDom k a w)

graft
  ∷ ∀ k k' a a' w w'
  . GraftX k k' a a' w w'
  → Graft k' a' w'
graft = unsafeCoerce

unGraft
  ∷ ∀ k' a' w' r
  . (∀ k a w. GraftX k k' a a' w w' → r)
  → Graft k' a' w'
  → r
unGraft f = f <<< unsafeCoerce

runGraft
  ∷ ∀ k' a' w'
  . Graft k' a' w'
  → VDom k' a' w'
runGraft =
  unGraft \(Graft fk fa fw v) →
    let
      go (Text s) = Text s
      go (Elem spec ch) = Elem (map fa spec) (map go ch)
      go (Keyed spec ch) = Keyed (map fa spec) (map (bimap fk go) ch)
      go (Widget w) = Widget (fw w)
      go (Grafted g) = Grafted (transformGraft fk fa fw g)
    in
      go v

transformVDom
  ∷ ∀ k k' a a' w w'
  . (k → k')
  → (a → a')
  → (w → w')
  → VDom k a w
  → VDom k' a' w'
transformVDom fk fa fw v =
  Grafted (graft (Graft fk fa fw v))

transformGraft
  ∷ ∀ k k' a a' w w'
  . (k → k')
  → (a → a')
  → (w → w')
  → Graft k a w
  → Graft k' a' w'
transformGraft fk fa fw =
  unGraft \(Graft fk' fa' fw' v) →
    graft (Graft (fk <<< fk') (fa <<< fa') (fw <<< fw') v)

data ElemSpec a = ElemSpec (Maybe Namespace) String a

instance functorElemSpec ∷ Functor ElemSpec where
  map f (ElemSpec ns name a) = ElemSpec ns name (f a)

newtype Namespace = Namespace String

derive instance eqNamespace ∷ Eq Namespace

unNamespace ∷ Namespace → String
unNamespace (Namespace s) = s

data Triple a b c = Triple a b c

type VDomEff eff = Eff (dom ∷ DOM | eff)

type VDomMachine eff a b = Machine (VDomEff eff) a b

type VDomStep eff a b = VDomEff eff (Step (VDomEff eff) a b)

newtype VDomSpec eff a w = VDomSpec
  { buildWidget ∷ VDomSpec eff a w → VDomMachine eff w DOM.Node
  , buildAttributes ∷ DOM.Element → VDomMachine eff a Unit
  , document ∷ DOM.Document
  }

buildVDom
  ∷ ∀ eff k a w
  . Ord k
  ⇒ VDomSpec eff a w
  → VDomMachine eff (VDom k a w) DOM.Node
buildVDom spec = render
  where
  render = case _ of
    Text s → buildText spec s
    Elem es ch → buildElem spec es ch
    Keyed es ch → buildKeyed spec es ch
    Widget w → buildWidget spec w
    Grafted g → buildVDom spec (runGraft g)

buildText
  ∷ ∀ eff k a w
  . Ord k
  ⇒ VDomSpec eff a w
  → String
  → VDomStep eff (VDom k a w) DOM.Node
buildText (VDomSpec spec) = render
  where
  render s = do
    node ← DOM.textToNode <$> DOM.createTextNode s spec.document
    pure (Step node (Fn.runFn2 patch node s) done)

  patch = Fn.mkFn2 \node s1 → case _ of
    Grafted g →
      Fn.runFn2 patch node s1 (runGraft g)
    Text s2 → do
      Fn.runFn2 whenE (s1 /= s2) do
        DOM.setTextContent s2 node
      pure (Step node (Fn.runFn2 patch node s2) done)
    vdom →
      buildVDom (VDomSpec spec) vdom

  done = pure unit

buildElem
  ∷ ∀ eff k a w
  . Ord k
  ⇒ VDomSpec eff a w
  → ElemSpec a
  → Array (VDom k a w)
  → VDomStep eff (VDom k a w) DOM.Node
buildElem (VDomSpec spec) = render
  where
  render es1@(ElemSpec ns1 name1 as1) ch1 = do
    el ←
      case map unNamespace ns1 of
        Nothing → DOM.createElement name1 spec.document
        ns → DOM.createElementNS (Null.toNullable ns) name1 spec.document
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
    Elem es2@(ElemSpec ns2 name2 as2) ch2 | ns1 == ns2 && name1 == name2 → do
      attrs' ← Machine.step attrs as2
      nodes ← DOM.childNodes node
      let
        onThese = Fn.mkFn3 \ix (Tuple step _) vdom → do
          Step n' m' h' ← step vdom
          let n = Fn.runFn2 unsafeChildIx ix node
          Fn.runFn2 whenE (not (Fn.runFn2 eqNode n' n)) do
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
  ∷ ∀ eff k a w
  . Ord k
  ⇒ VDomSpec eff a w
  → ElemSpec a
  → Array (Tuple k (VDom k a w))
  → VDomStep eff (VDom k a w) DOM.Node
buildKeyed (VDomSpec spec) = render
  where
  render es1@(ElemSpec ns1 name1 as1) ch1 = do
    el ←
      case map unNamespace ns1 of
        Nothing → DOM.createElement name1 spec.document
        ns → DOM.createElementNS (Null.toNullable ns) name1 spec.document
    attrs ← spec.buildAttributes el as1
    let
      node = DOM.elementToNode el
      onChild = Fn.mkFn3 \ix acc (Tuple k child) → do
        Step n m h ← buildVDom (VDomSpec spec) child
        Fn.runFn2 appendChild n node
        pure (Map.insert k (Triple ix m h) acc)
    steps ← Fn.runFn3 foldWithIxE Map.empty ch1 onChild
    pure
      (Step node
        (Fn.runFn4 patch node attrs es1 steps)
        (Fn.runFn2 done attrs steps))

  patch = Fn.mkFn4 \node attrs (es1@(ElemSpec ns1 name1 as1)) ch1 → case _ of
    Grafted g →
      Fn.runFn4 patch node attrs es1 ch1 (runGraft g)
    Keyed es2@(ElemSpec ns2 name2 as2) ch2 | ns1 == ns2 && name1 == name2 → do
      attrs' ← Machine.step attrs as2
      let
        onChild = Fn.mkFn3 \ix (Tuple old new) (Tuple k child) →
          case Map.pop k old of
            Just (Tuple (Triple ix' step _) old') | ix == ix' → do
              Step n' m' h' ← step child
              let n = Fn.runFn2 unsafeChildIx ix node
              Fn.runFn2 whenE (not (Fn.runFn2 eqNode n' n))
                (Fn.runFn3 replaceChild n' n node)
              pure (Tuple old' (Map.insert k (Triple ix m' h') new))
            Just (Tuple (Triple ix' step _) old') → do
              Step n' m' h' ← step child
              Fn.runFn3 unsafeInsertChildIx ix n' node
              pure (Tuple old' (Map.insert k (Triple ix m' h') new))
            Nothing → do
              Step n' m' h' ← buildVDom (VDomSpec spec) child
              Fn.runFn3 unsafeInsertChildIx ix n' node
              pure (Tuple old (Map.insert k (Triple ix m' h') new))
      Tuple old steps ← Fn.runFn3 foldWithIxE (Tuple ch1 Map.empty) ch2 onChild
      Fn.runFn2 forE
        (Array.fromFoldable old)
        (Fn.mkFn2 \i (Triple _ _ halt) → halt)
      removeDanglingNodes (Array.length ch2) node
      pure
        (Step node
          (Fn.runFn4 patch node attrs' es2 steps)
          (Fn.runFn2 done attrs' steps))
    vdom →
      buildVDom (VDomSpec spec) vdom

  done = Fn.mkFn2 \attrs steps → do
    Fn.runFn2 forE
      (Array.fromFoldable steps)
      (Fn.mkFn2 \i (Triple _ _ halt) → halt)
    Machine.halt attrs

buildWidget
  ∷ ∀ eff k a w
  . Ord k
  ⇒ VDomSpec eff a w
  → w
  → VDomStep eff (VDom k a w) DOM.Node
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

removeDanglingNodes ∷ ∀ eff. Int → DOM.Node → Eff (dom ∷ DOM | eff) Unit
removeDanglingNodes len node =
  Fn.runFn2 replicateE ((unsafeChildLength node) - len)
    (removeFirstChild node)

foreign import forE
  ∷ ∀ eff a b
  . Fn.Fn2
      (Array a)
      (Fn.Fn2 Int a (Eff eff b))
      (Eff eff (Array b))

foreign import foldWithIxE
  ∷ ∀ eff a b
  . Fn.Fn3
      b
      (Array a)
      (Fn.Fn3 Int b a (Eff eff b))
      (Eff eff b)

foreign import replicateE
  ∷ ∀ eff a
  . Fn.Fn2 Int (Eff eff a) (Eff eff Unit)

foreign import whenE
  ∷ ∀ eff a
  . Fn.Fn2 Boolean (Eff eff a) (Eff eff Unit)

foreign import diffWithIxE
  ∷ ∀ eff b c d
  . Fn.Fn5
      (Array b)
      (Array c)
      (Fn.Fn3 Int b c (Eff eff d))
      (Fn.Fn2 Int b (Eff eff Unit))
      (Fn.Fn2 Int c (Eff eff d))
      (Eff eff (Array d))

foreign import replaceChild
  ∷ ∀ eff
  . Fn.Fn3 DOM.Node DOM.Node DOM.Node (Eff (dom ∷ DOM | eff) Unit)

foreign import removeFirstChild
  ∷ ∀ eff
  . DOM.Node → (Eff (dom ∷ DOM | eff) Unit)

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
  ∷ Fn.Fn2 Int DOM.Node DOM.Node

foreign import unsafeChildLength
  ∷ DOM.Node → Int

foreign import eqNode
  ∷ Fn.Fn2 DOM.Node DOM.Node Boolean
