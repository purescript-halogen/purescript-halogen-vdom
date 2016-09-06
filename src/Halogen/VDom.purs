module Halogen.VDom where

import Prelude
import Data.Bifunctor (bimap)
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Nullable as Null
import Data.Tuple (Tuple(..), snd)
import Control.Monad.Eff (Eff, foreachE)
import Unsafe.Coerce (unsafeCoerce)

import DOM (DOM)
import DOM.Node.Document (createElement, createElementNS, createTextNode) as DOM
import DOM.Node.Types (Element, Node, Document, NodeList, elementToNode, textToNode) as DOM
import DOM.Node.Node (appendChild, replaceChild, removeChild, childNodes, setTextContent) as DOM
import DOM.Node.NodeList (item) as DOM

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

elem
  ∷ ∀ k a w
  . String
  → Array a
  → Array (VDom k (Array a) w)
  → VDom k (Array a) w
elem name attrs =
  Elem (ElemSpec Nothing name attrs)

text ∷ ∀ k a w. String → VDom k a w
text = Text

newtype Namespace = Namespace String

derive instance eqNamespace ∷ Eq Namespace

unNamespace ∷ Namespace → String
unNamespace (Namespace s) = s

nsSVG ∷ Namespace
nsSVG = Namespace "http://www.w3.org/2000/svg"

type VDomEff eff = Eff (dom ∷ DOM | eff)

type VDomMachine eff a b = Machine (VDomEff eff) a b

type VDomStep eff a b = VDomEff eff (Step (VDomEff eff) a b)

type VDomSpec eff a w =
  { buildWidget ∷ VDomMachine eff w DOM.Node
  , buildAttributes ∷ DOM.Element → VDomMachine eff a Unit
  , document ∷ DOM.Document
  }

buildVDom
  ∷ ∀ eff k a w
  . VDomSpec eff a w
  → VDomMachine eff (VDom k a w) DOM.Node
buildVDom spec = render
  where
  render = case _ of
    Text s → buildText spec s
    Elem es ch → buildElem spec es ch
    Keyed es ch → buildElem spec es (map snd ch) -- TODO
    Widget w → buildWidget spec w
    Grafted g → buildVDom spec (runGraft g)

buildText
  ∷ ∀ eff k a w
  . VDomSpec eff a w
  → String
  → VDomStep eff (VDom k a w) DOM.Node
buildText spec = render
  where
  render s = do
    node ← DOM.textToNode <$> DOM.createTextNode s spec.document
    pure (Step node (patch node s) done)

  patch node s1 = case _ of
    Grafted g →
      patch node s1 (runGraft g)
    Text s2 → do
      when (s1 /= s2) (DOM.setTextContent s2 node)
      pure (Step node (patch node s2) done)
    vdom →
      buildVDom spec vdom

  done = pure unit

buildElem
  ∷ ∀ eff k a w
  . VDomSpec eff a w
  → ElemSpec a
  → Array (VDom k a w)
  → VDomStep eff (VDom k a w) DOM.Node
buildElem spec = render
  where
  render es1@(ElemSpec ns1 name1 as1) ch1 = do
    el ←
      case map unNamespace ns1 of
        Nothing → DOM.createElement name1 spec.document
        ns → DOM.createElementNS (Null.toNullable ns) name1 spec.document
    let node = DOM.elementToNode el
    attrs ← spec.buildAttributes el as1
    steps ← Fn.runFn2 forE ch1 \child → do
      Step n m h ← buildVDom spec child
      DOM.appendChild n node
      pure (Tuple m h)
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
          n ← unsafeItem ix nodes
          when (not (n `eqNode` n')) do
            void (DOM.replaceChild n' n node)
          pure (Tuple m' h')

        onThis = Fn.mkFn2 \ix (Tuple _ halt) → void do
          halt
          n ← unsafeItem ix nodes
          DOM.removeChild n node

        onThat = Fn.mkFn2 \ix vdom → do
          Step n m h ← buildVDom spec vdom
          DOM.appendChild n node
          pure (Tuple m h)
      steps ← Fn.runFn5 diffWithIxE ch1 ch2 onThese onThis onThat
      pure
        (Step node
          (Fn.runFn4 patch node attrs' es2 steps)
          (Fn.runFn2 done attrs' steps))
    vdom →
      buildVDom spec vdom

  done = Fn.mkFn2 \attrs steps → do
    foreachE steps snd
    Machine.halt attrs

buildWidget
  ∷ ∀ eff k a w
  . VDomSpec eff a w
  → w
  → VDomStep eff (VDom k a w) DOM.Node
buildWidget spec = render
  where
  render w = do
    Step n m h ← spec.buildWidget w
    pure (Step n (patch m) h)

  patch step = case _ of
    Grafted g →
      patch step (runGraft g)
    Widget w → do
      Step n m h ← step w
      pure (Step n (patch m) h)
    vdom →
      buildVDom spec vdom

unsafeItem ∷ ∀ eff. Int → DOM.NodeList → Eff (dom ∷ DOM | eff) DOM.Node
unsafeItem = unsafeCoerce DOM.item

foreign import forE
  ∷ ∀ eff a b
  . Fn.Fn2
      (Array a)
      (a → Eff eff b)
      (Eff eff (Array b))

foreign import diffWithIxE
  ∷ ∀ eff b c d
  . Fn.Fn5
      (Array b)
      (Array c)
      (Fn.Fn3 Int b c (Eff eff d))
      (Fn.Fn2 Int b (Eff eff Unit))
      (Fn.Fn2 Int c (Eff eff d))
      (Eff eff (Array d))

foreign import eqNode ∷ DOM.Node → DOM.Node → Boolean
