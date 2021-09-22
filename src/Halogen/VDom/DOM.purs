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
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (toNullable)
import Data.Tuple (Tuple(..), fst)
import Effect.Uncurried as EFn
import Foreign (Foreign)
import Foreign.Object as Object
import Halogen.VDom.Machine (Machine, Step, Step'(..), extract, halt, mkStep, step, unStep)
import Halogen.VDom.Machine as Machine
import Halogen.VDom.Types (ElemName(..), FnObject, Namespace(..), VDom(..), runGraft)
import Halogen.VDom.Util as Util
import Web.DOM.Element (Element) as DOM
import Web.DOM.Element as DOMElement
import Web.DOM.Node (Node) as DOM

type VDomMachine a w = Machine (VDom a w) DOM.Node

type VDomStep a w = Step (VDom a w) DOM.Node

type VDomInit i a w = EFn.EffectFn1 i (VDomStep a w)

type VDomBuilder i a w = EFn.EffectFn3 (VDomSpec a w) (VDomMachine a w) i (VDomStep a w)

type VDomBuilder4 i j k l a w = EFn.EffectFn6 (VDomSpec a w) (VDomMachine a w) i j k l (VDomStep a w)

type VDomBuilder5 i j a w ch = EFn.EffectFn5 (VDomSpec a w) (VDomMachine a w) i j ch (VDomStep a w)

type VDomBuilder6 i j a w = EFn.EffectFn4 (VDomSpec a w) (VDomMachine a w) i j (VDomStep a w)

-- | Widget machines recursively reference the configured spec to potentially
-- | enable recursive trees of Widgets.
newtype VDomSpec a w = VDomSpec
  { buildWidget ∷ VDomSpec a w → Machine w DOM.Node
  , buildAttributes ∷ FnObject -> DOM.Element → Machine a Unit
  , fnObject :: FnObject
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
    Elem ns n a ch → EFn.runEffectFn6 buildElem spec build ns n a ch
    Keyed ns n a ch → EFn.runEffectFn6 buildKeyed spec build ns n a ch
    Widget w → EFn.runEffectFn3 buildWidget spec build w
    Grafted g → EFn.runEffectFn1 build (runGraft g)
    Microapp s g ch → EFn.runEffectFn5 buildMicroapp spec build s g ch

type MicroAppState a w =
  { build ∷ VDomMachine a w
  , node ∷ DOM.Node
  , requestId :: String
  , attrs ∷ Step a Unit
  , service :: String
  , payload :: Maybe Foreign
  , children :: Array (VDomStep a w)
  }

buildMicroapp ∷ ∀ a w. VDomBuilder5 String a a w (Maybe (Array (VDom a w)))
buildMicroapp = EFn.mkEffectFn5 \(VDomSpec spec) build s as1 ch → do
  -- GET ID, SCHEDULE AN AFTER RENDER CALL TO M-APP
  -- MAYBE ADD A FUNCTION FROM PRESTO_DOM TO SCHEDULE
  requestId <- Util.generateUUID
  el ← EFn.runEffectFn3 Util.createMicroapp spec.fnObject requestId s
  let node = DOMElement.toNode el
  attrs ← EFn.runEffectFn1 (spec.buildAttributes spec.fnObject el) as1
  let onChild = EFn.mkEffectFn2 \ix child → do
                res ← EFn.runEffectFn1 build child
                EFn.runEffectFn5 Util.insertChildIx spec.fnObject "render" ix (extract res) node
                pure res
  children ← EFn.runEffectFn2 Util.forE (fromMaybe [] ch) onChild
  let state = { build, node, service: s, attrs, requestId : requestId, payload : Nothing, children }
  pure $ mkStep $ Step node state (patchMicroapp spec.fnObject) (haltMicroapp spec.fnObject)

patchMicroapp ∷ ∀ a w. FnObject -> EFn.EffectFn2 (MicroAppState a w) (VDom a w) (VDomStep a w)
patchMicroapp fnObject = EFn.mkEffectFn2 \state vdom → do
  let { build, node, attrs, service: value1, requestId, payload, children : ch1} = state
  case vdom of
    Grafted g →
      EFn.runEffectFn2 (patchMicroapp fnObject) state (runGraft g)
    Microapp s2 value2 ch2 -- CHANGE IN PAYLOAD, NEEDS TO TERMINATE OLD / FIRE EVENT TO OTHER M_APP
      | value1 == s2 → do
          let
            onThese = EFn.mkEffectFn4 \obj ix s v → do
              res ← EFn.runEffectFn2 step s v
              EFn.runEffectFn5 Util.insertChildIx obj "patch" ix (extract res) node
              pure res
            onThis = EFn.mkEffectFn3 \obj ix s → EFn.runEffectFn1 halt s
            onThat = EFn.mkEffectFn3 \obj ix v → do
              res ← EFn.runEffectFn1 build v
              EFn.runEffectFn5 Util.insertChildIx obj "patch" ix (extract res) node
              pure res
          children2 ← EFn.runEffectFn6 Util.diffWithIxE fnObject ch1 (fromMaybe [] ch2) onThese onThis onThat
          attrs2 ← EFn.runEffectFn2 step attrs value2
          pure $ mkStep $ Step node (state {attrs = attrs2, children= children2}) (patchMicroapp fnObject) (haltMicroapp fnObject)
      | otherwise → do
          -- NOT HANDLED THIS IS DUMMY CODE
          -- CASE WHERE SERVICE CHANGES IS NOT ACCEPTABLE [FOR NOW]
          -- DOING NOTHING
          pure $ mkStep $ Step node state (patchMicroapp fnObject) (haltMicroapp fnObject)
    _ → do
      EFn.runEffectFn1 (haltMicroapp fnObject) state
      EFn.runEffectFn1 build vdom

haltMicroapp ∷ ∀ a w. FnObject -> EFn.EffectFn1 (MicroAppState a w) Unit
haltMicroapp fnObject = EFn.mkEffectFn1 \{ node, attrs, children } → do
  parent ← EFn.runEffectFn1 Util.parentNode node
  EFn.runEffectFn3 Util.removeChild fnObject node parent
  EFn.runEffectFn2 Util.forEachE children halt
  EFn.runEffectFn1 halt attrs

type TextState a w =
  { build ∷ VDomMachine a w
  , node ∷ DOM.Node
  , value ∷ String
  }

buildText ∷ ∀ a w. VDomBuilder String a w
buildText = EFn.mkEffectFn3 \(VDomSpec spec) build s → do
  node ← EFn.runEffectFn1 Util.createTextNode s
  let state = { build, node, value: s }
  pure $ mkStep $ Step node state (patchText spec.fnObject) (haltText spec.fnObject)

patchText ∷ ∀ a w. FnObject -> EFn.EffectFn2 (TextState a w) (VDom a w) (VDomStep a w)
patchText fnObject = EFn.mkEffectFn2 \state vdom → do
  let { build, node, value: value1 } = state
  case vdom of
    Grafted g →
      EFn.runEffectFn2 (patchText fnObject) state (runGraft g)
    Text value2
      | value1 == value2 →
          pure $ mkStep $ Step node state (patchText fnObject) (haltText fnObject)
      | otherwise → do
          let nextState = { build, node, value: value2 }
          EFn.runEffectFn2 Util.setTextContent value2 node
          pure $ mkStep $ Step node nextState (patchText fnObject) (haltText fnObject)
    _ → do
      EFn.runEffectFn1 (haltText fnObject) state
      EFn.runEffectFn1 build vdom

haltText ∷ ∀ a w. FnObject -> EFn.EffectFn1 (TextState a w) Unit
haltText fnObject = EFn.mkEffectFn1 \{ node } → do
  parent ← EFn.runEffectFn1 Util.parentNode node
  EFn.runEffectFn3 Util.removeChild fnObject node parent

type ElemState a w =
  { build ∷ VDomMachine a w
  , node ∷ DOM.Node
  , attrs ∷ Step a Unit
  , ns ∷ Maybe Namespace
  , name ∷ ElemName
  , children ∷ Array (VDomStep a w)
  }

buildElem ∷ ∀ a w. VDomBuilder4 (Maybe Namespace) ElemName a (Array (VDom a w)) a w
buildElem = EFn.mkEffectFn6 \(VDomSpec spec) build ns1 name1 as1 ch1 → do
  el ← EFn.runEffectFn3 Util.createElement spec.fnObject (toNullable ns1) name1
  let
    node = DOMElement.toNode el
    onChild = EFn.mkEffectFn2 \ix child → do
      res ← EFn.runEffectFn1 build child
      EFn.runEffectFn5 Util.insertChildIx spec.fnObject "render" ix (extract res) node
      pure res
  children ← EFn.runEffectFn2 Util.forE ch1 onChild
  attrs ← EFn.runEffectFn1 (spec.buildAttributes spec.fnObject el) as1
  let
    state =
      { build
      , node
      , attrs
      , ns: ns1
      , name: name1
      , children
      }
  pure $ mkStep $ Step node state (patchElem spec.fnObject) (haltElem spec.fnObject)

patchElem ∷ ∀ a w. FnObject -> EFn.EffectFn2 (ElemState a w) (VDom a w) (VDomStep a w)
patchElem fnObject = EFn.mkEffectFn2 \state vdom → do
  let { build, node, attrs, ns: ns1, name: name1, children: ch1 } = state
  case vdom of
    Grafted g →
      EFn.runEffectFn2 (patchElem fnObject) state (runGraft g)
    Elem ns2 name2 as2 ch2 | Fn.runFn4 eqElemSpec ns1 name1 ns2 name2 → do
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
          pure $ mkStep $ Step node nextState (patchElem fnObject) (haltElem fnObject)
        _, _ → do
          let
            onThese = EFn.mkEffectFn4 \obj ix s v → do
              res ← EFn.runEffectFn2 step s v
              EFn.runEffectFn5 Util.insertChildIx obj "patch" ix (extract res) node
              pure res
            onThis = EFn.mkEffectFn3 \obj ix s → EFn.runEffectFn1 halt s
            onThat = EFn.mkEffectFn3 \obj ix v → do
              res ← EFn.runEffectFn1 build v
              EFn.runEffectFn5 Util.insertChildIx obj "patch" ix (extract res) node
              pure res
          children2 ← EFn.runEffectFn6 Util.diffWithIxE fnObject ch1 ch2 onThese onThis onThat
          attrs2 ← EFn.runEffectFn2 step attrs as2
          let
            nextState =
              { build
              , node
              , attrs: attrs2
              , ns: ns2
              , name: name2
              , children: children2
              }
          pure $ mkStep $ Step node nextState (patchElem fnObject) (haltElem fnObject)
    _ → do
      EFn.runEffectFn1 (haltElem fnObject) state
      EFn.runEffectFn1 build vdom

haltElem ∷ ∀ a w. FnObject -> EFn.EffectFn1 (ElemState a w) Unit
haltElem fnObject = EFn.mkEffectFn1 \{ node, attrs, children } → do
  parent ← EFn.runEffectFn1 Util.parentNode node
  EFn.runEffectFn3 Util.removeChild fnObject node parent
  EFn.runEffectFn2 Util.forEachE children halt
  EFn.runEffectFn1 halt attrs

type KeyedState a w =
  { build ∷ VDomMachine a w
  , node ∷ DOM.Node
  , attrs ∷ Step a Unit
  , ns ∷ Maybe Namespace
  , name ∷ ElemName
  , children ∷ Object.Object (VDomStep a w)
  , length ∷ Int
  }

buildKeyed ∷ ∀ a w. VDomBuilder4 (Maybe Namespace) ElemName a (Array (Tuple String (VDom a w))) a w
buildKeyed = EFn.mkEffectFn6 \(VDomSpec spec) build ns1 name1 as1 ch1 → do
  el ← EFn.runEffectFn3 Util.createElement spec.fnObject (toNullable ns1) name1
  let
    node = DOMElement.toNode el
    onChild = EFn.mkEffectFn4 \k ix _ (Tuple _ vdom) → do
      res ← EFn.runEffectFn1 build vdom
      EFn.runEffectFn5 Util.insertChildIx spec.fnObject "render" ix (extract res) node
      pure res
  children ← EFn.runEffectFn3 Util.strMapWithIxE ch1 fst onChild
  attrs ← EFn.runEffectFn1 (spec.buildAttributes spec.fnObject el) as1
  let
    state =
      { build
      , node
      , attrs
      , ns: ns1
      , name: name1
      , children
      , length: Array.length ch1
      }
  pure $ mkStep $ Step node state (patchKeyed spec.fnObject) (haltKeyed spec.fnObject)

patchKeyed ∷ ∀ a w. FnObject -> EFn.EffectFn2 (KeyedState a w) (VDom a w) (VDomStep a w)
patchKeyed fnObject = EFn.mkEffectFn2 \state vdom → do
  let { build, node, attrs, ns: ns1, name: name1, children: ch1, length: len1 } = state
  case vdom of
    Grafted g →
      EFn.runEffectFn2 (patchKeyed fnObject) state (runGraft g)
    Keyed ns2 name2 as2 ch2 | Fn.runFn4 eqElemSpec ns1 name1 ns2 name2 →
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
              , children: ch1
              , length: 0
              }
          pure $ mkStep $ Step node nextState (patchKeyed fnObject) (haltKeyed fnObject)
        _, len2 → do
          let
            onThese = EFn.mkEffectFn5 \obj _ ix' s (Tuple _ v) → do
              res ← EFn.runEffectFn2 step s v
              EFn.runEffectFn5 Util.insertChildIx obj "patch" ix' (extract res) node
              pure res
            onThis = EFn.mkEffectFn3 \obj _ s → EFn.runEffectFn1 halt s
            onThat = EFn.mkEffectFn4 \obj _ ix (Tuple _ v) → do
              res ← EFn.runEffectFn1 build v
              EFn.runEffectFn5 Util.insertChildIx obj "patch" ix (extract res) node
              pure res
          children2 ← EFn.runEffectFn7 Util.diffWithKeyAndIxE fnObject ch1 ch2 fst onThese onThis onThat
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
          pure $ mkStep $ Step node nextState (patchKeyed fnObject) (haltKeyed fnObject)
    _ → do
      EFn.runEffectFn1 (haltKeyed fnObject) state
      EFn.runEffectFn1 build vdom

haltKeyed ∷ ∀ a w. FnObject -> EFn.EffectFn1 (KeyedState a w) Unit
haltKeyed fnObject = EFn.mkEffectFn1 \{ node, attrs, children } → do
  parent ← EFn.runEffectFn1 Util.parentNode node
  EFn.runEffectFn3 Util.removeChild fnObject node parent
  EFn.runEffectFn2 Util.forInE children (EFn.mkEffectFn2 \_ s → EFn.runEffectFn1 halt s)
  EFn.runEffectFn1 halt attrs

type WidgetState a w =
  { build ∷ VDomMachine a w
  , widget ∷ Step w DOM.Node
  }

buildWidget ∷ ∀ a w. VDomBuilder w a w
buildWidget = EFn.mkEffectFn3 \(VDomSpec spec) build w → do
  res ← EFn.runEffectFn1 (spec.buildWidget (VDomSpec spec)) w
  let
    res' = res # unStep \(Step n s k1 k2) →
      mkStep $ Step n { build, widget: res } patchWidget haltWidget
  pure res'

patchWidget ∷ ∀ a w. EFn.EffectFn2 (WidgetState a w) (VDom a w) (VDomStep a w)
patchWidget = EFn.mkEffectFn2 \state vdom → do
  let { build, widget } = state
  case vdom of
    Grafted g →
      EFn.runEffectFn2 patchWidget state (runGraft g)
    Widget w → do
      res ← EFn.runEffectFn2 step widget w
      let
        res' = res # unStep \(Step n s k1 k2) →
          mkStep $ Step n { build, widget: res } patchWidget haltWidget
      pure res'
    _ → do
      EFn.runEffectFn1 haltWidget state
      EFn.runEffectFn1 build vdom

haltWidget ∷ forall a w. EFn.EffectFn1 (WidgetState a w) Unit
haltWidget = EFn.mkEffectFn1 \{ widget } → do
  EFn.runEffectFn1 halt widget

eqElemSpec ∷ Fn.Fn4 (Maybe Namespace) ElemName (Maybe Namespace) ElemName Boolean
eqElemSpec = Fn.mkFn4 \ns1 (ElemName name1) ns2 (ElemName name2) →
  if name1 == name2
    then case ns1, ns2 of
      Just (Namespace ns1'), Just (Namespace ns2') | ns1' == ns2' → true
      Nothing, Nothing → true
      _, _ → false
    else false
