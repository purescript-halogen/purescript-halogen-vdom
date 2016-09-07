module Halogen.VDom where

import Prelude
import Data.Array as Array
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Nullable as Null
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..), fst, snd)
import Control.Monad.Eff (Eff, foreachE)
import Unsafe.Coerce (unsafeCoerce)

import DOM (DOM)
import DOM.Node.Document (createElement, createElementNS, createTextNode) as DOM
import DOM.Node.Types (Element, Node, Document, elementToNode, textToNode) as DOM
import DOM.Node.Node (childNodes, setTextContent) as DOM

import Halogen.VDom.Machine (Step(..), Machine)
import Halogen.VDom.Machine as Machine

data VDom a w
  = Text String
  | Elem (ElemSpec a) (Array (VDom a w))
  | Keyed (ElemSpec a) (Array (Tuple String (VDom a w)))
  | Widget w
  | Grafted (Graft a w)

instance functorVDom ∷ Functor (VDom a) where
  map f (Text s) = Text s
  map f (Grafted g) = Grafted (map f g)
  map f v = Grafted (graft (Graft id f v))

foreign import data Graft ∷ * → * → *

instance functorGraft ∷ Functor (Graft a) where
  map f = unGraft \(Graft fa fw v) → graft (Graft fa (f <<< fw) v)

data GraftX a a' w w' =
  Graft (a → a') (w → w') (VDom a w)

graft
  ∷ ∀ a a' w w'
  . GraftX a a' w w'
  → Graft a' w'
graft = unsafeCoerce

unGraft
  ∷ ∀ a' w' r
  . (∀ a w. GraftX a a' w w' → r)
  → Graft a' w'
  → r
unGraft f = f <<< unsafeCoerce

runGraft
  ∷ ∀ a' w'
  . Graft a' w'
  → VDom a' w'
runGraft =
  unGraft \(Graft fa fw v) →
    let
      go (Text s) = Text s
      go (Elem spec ch) = Elem (map fa spec) (map go ch)
      go (Keyed spec ch) = Keyed (map fa spec) (map (map go) ch)
      go (Widget w) = Widget (fw w)
      go (Grafted g) = Grafted (transformGraft fa fw g)
    in
      go v

transformVDom
  ∷ ∀ a a' w w'
  . (a → a')
  → (w → w')
  → VDom a w
  → VDom a' w'
transformVDom fa fw v =
  Grafted (graft (Graft fa fw v))

transformGraft
  ∷ ∀ a a' w w'
  . (a → a')
  → (w → w')
  → Graft a w
  → Graft a' w'
transformGraft fa fw =
  unGraft \(Graft fa' fw' v) →
    graft (Graft (fa <<< fa') (fw <<< fw') v)

data ElemSpec a = ElemSpec (Maybe Namespace) String a

instance functorElemSpec ∷ Functor ElemSpec where
  map f (ElemSpec ns name a) = ElemSpec ns name (f a)

newtype Namespace = Namespace String

derive instance eqNamespace ∷ Eq Namespace

unNamespace ∷ Namespace → String
unNamespace (Namespace s) = s

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
  ∷ ∀ eff a w
  . VDomSpec eff a w
  → ElemSpec a
  → Array (VDom a w)
  → VDomStep eff (VDom a w) DOM.Node
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
  ∷ ∀ eff a w
  . VDomSpec eff a w
  → ElemSpec a
  → Array (Tuple String (VDom a w))
  → VDomStep eff (VDom a w) DOM.Node
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
    Keyed es2@(ElemSpec ns2 name2 as2) ch2 | ns1 == ns2 && name1 == name2 → do
      attrs' ← Machine.step attrs as2
      let
        onThese = Fn.mkFn4 \k ix (Quaple _ ix' step _) (Tuple _ vdom) →
          if ix == ix'
            then do
              Step n' m' h' ← step vdom
              let n = Fn.runFn2 unsafeChildIx ix node
              Fn.runFn2 whenE (not (Fn.runFn2 eqNode n' n))
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
    Fn.runFn2 forE
      (Array.fromFoldable steps)
      (Fn.mkFn2 \i (Quaple _ _ _ halt) → halt)
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

foreign import forE
  ∷ ∀ eff a b
  . Fn.Fn2
      (Array a)
      (Fn.Fn2 Int a (Eff eff b))
      (Eff eff (Array b))

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

foreign import strMapWithIxE
  ∷ ∀ eff a b
  . Fn.Fn3
      (Array a)
      (a → String)
      (Fn.Fn3 String Int a (Eff eff b))
      (Eff eff (StrMap.StrMap b))

foreign import diffWithKeyAndIxE
  ∷ ∀ eff a b c d
  . Fn.Fn6
      (StrMap.StrMap a)
      (Array b)
      (b → String)
      (Fn.Fn4 String Int a b (Eff eff c))
      (Fn.Fn2 String a (Eff eff d))
      (Fn.Fn3 String Int b (Eff eff c))
      (Eff eff (StrMap.StrMap c))
