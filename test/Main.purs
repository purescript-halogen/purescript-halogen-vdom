module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Eff.Timer as Timer
import Data.Exists (Exists, mkExists, runExists)
import Data.Maybe (Maybe(..), isNothing)
import Data.Nullable (toMaybe)
import Data.Foldable (for_, traverse_)
import Data.Function.Uncurried as Fn
import Data.Tuple (Tuple(..), fst)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument, htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.Element (setAttribute, removeAttribute) as DOM
import DOM.Node.Types (Document, Element, Node, elementToNode) as DOM
import DOM.Node.Node (appendChild) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import Halogen.VDom as V
import Halogen.VDom.Util (whenE, refEq, diffWithKeyAndIxE, strMapWithIxE)
import Unsafe.Coerce (unsafeCoerce)

infixr 1 Tuple as :=

type Attribute = Tuple String String

type VDom = V.VDom (Array Attribute) (Exists Thunk)

data Thunk b = Thunk b (b → VDom)

type State = Array Database

type Database =
  { dbname ∷ String
  , lastSample ∷ LastSample
  }

type LastSample =
  { countClassName ∷ String
  , nbQueries ∷ Int
  , topFiveQueries ∷ Array DBQuery
  }

type DBQuery =
  { elapsedClassName ∷ String
  , formatElapsed ∷ String
  , query ∷ String
  }

initialState ∷ State
initialState = []

elem ∷ ∀ a w. String → a → Array (V.VDom a w) → V.VDom a w
elem n a = V.Elem (V.ElemSpec Nothing (V.ElemName n) a)

keyed ∷ ∀ a w. String → a → Array (Tuple String (V.VDom a w)) → V.VDom a w
keyed n a = V.Keyed (V.ElemSpec Nothing (V.ElemName n) a)

text ∷ ∀ a w. String → V.VDom a w
text = V.Text

thunk ∷ ∀ a. (a → VDom) → a → VDom
thunk render val = V.Widget (mkExists (Thunk val render))

renderData ∷ State → VDom
renderData st =
  elem "div" []
    [ elem "table"
        [ "class" := "table table-striped latest data" ]
        -- [ keyed "tbody" [] (map (\db → Tuple db.dbname (thunk renderDatabase db)) st) ]
        -- [ keyed "tbody" [] (map (\db → Tuple db.dbname (renderDatabase db)) st) ]
        -- [ elem "tbody" [] (map (thunk renderDatabase) st) ]
        [ elem "tbody" [] (map renderDatabase st) ]
    ]

  where
  renderDatabase db =
    elem "tr"
      []
      ([ elem "td"
          [ "class" := "dbname" ]
          [ text db.dbname ]
      , elem "td"
          [ "class" := "query-count" ]
          [ elem "span"
              [ "class" := db.lastSample.countClassName ]
              [ text (show db.lastSample.nbQueries) ]
          ]
      ] <> map renderQuery db.lastSample.topFiveQueries)

  renderQuery q =
    elem "td"
      [ "class" := "Query" <> q.elapsedClassName ]
      [ text q.formatElapsed
      , elem "div"
          [ "class" := "popover left" ]
          [ elem "div"
              [ "class" := "popover-content" ]
              [ text q.query ]
          , elem "div"
              [ "class" := "arrow" ]
              []
          ]
      ]

buildWidget
  ∷ ∀ eff
  . V.VDomSpec (dom ∷ DOM | eff) (Array Attribute) (Exists Thunk)
  → V.VDomMachine (dom ∷ DOM | eff) (Exists Thunk) DOM.Node
buildWidget spec = render
  where
  render = runExists \(Thunk a render) → do
    V.Step node m h ← V.buildVDom spec (render a)
    pure (V.Step node (Fn.runFn4 patch (unsafeCoerce a) node m h) h)

  patch = Fn.mkFn4 \a node step halt → runExists \(Thunk b render) →
    if Fn.runFn2 refEq a b
      then pure (V.Step node (Fn.runFn4 patch a node step halt) halt)
      else do
        V.Step node' m h ← step (render b)
        pure (V.Step node (Fn.runFn4 patch (unsafeCoerce b) node' m h) h)

buildAttributes
  ∷ ∀ eff
  . DOM.Element
  → V.VDomMachine (dom ∷ DOM | eff) (Array Attribute) Unit
buildAttributes el = render
  where
  render as1 = do
    let
      onAttr = Fn.mkFn3 \k _ (Tuple _ v) → do
        DOM.setAttribute k v el
        pure v
    as1' ← Fn.runFn3 strMapWithIxE as1 fst onAttr
    pure (V.Step unit (patch as1') done)

  patch as1 as2 = do
    let
      onThese = Fn.mkFn4 \k _ a (Tuple _ b) → do
        Fn.runFn2 whenE (a /= b)
          (DOM.setAttribute k b el)
        pure b
      onThis = Fn.mkFn2 \k _ → DOM.removeAttribute k el
      onThat = Fn.mkFn3 \k ix (Tuple _ a) → do
        DOM.setAttribute k a el
        pure a
    as2' ← Fn.runFn6 diffWithKeyAndIxE as1 as2 fst onThese onThis onThat
    pure (V.Step unit (patch as1) done)

  done = pure unit

mkSpec
  ∷ ∀ eff
  . DOM.Document
  → V.VDomSpec (dom ∷ DOM | eff) (Array Attribute) (Exists Thunk)
mkSpec document = V.VDomSpec
  { buildWidget
  , buildAttributes
  , document
  }

foreign import data DBMON ∷ !

foreign import getData ∷ ∀ eff. Eff (dbmon ∷ DBMON | eff) State

foreign import getTimeout ∷ ∀ eff. Eff (dbmon ∷ DBMON | eff) Int

foreign import pingRenderRate ∷ ∀ eff. Eff (dbmon ∷ DBMON | eff) Unit

foreign import requestAnimationFrame ∷ ∀ eff. Eff (dom ∷ DOM | eff) Unit → Eff (dom ∷ DOM | eff) Unit

mkRenderQueue
  ∷ ∀ eff a
  . V.VDomSpec (dom ∷ DOM, ref ∷ REF | eff) (Array Attribute) (Exists Thunk)
  → DOM.Node
  → (a → VDom)
  → a
  → Eff (dom ∷ DOM, ref ∷ REF | eff) (a → Eff (dom ∷ DOM, ref ∷ REF | eff) Unit)
mkRenderQueue spec parent render initialValue = do
  initMachine ← V.buildVDom spec (render initialValue)
  DOM.appendChild (V.extract initMachine) parent
  ref ← Ref.newRef initMachine
  val ← Ref.newRef Nothing
  pure \a → do
    v ← Ref.readRef val
    Ref.writeRef val (Just a)
    when (isNothing v) $ requestAnimationFrame do
      machine ← Ref.readRef ref
      Ref.readRef val >>= traverse_ \v' → do
        res ← V.step machine (render v')
        Ref.writeRef ref res
        Ref.writeRef val Nothing

mkRenderQueue'
  ∷ ∀ eff a
  . V.VDomSpec (dom ∷ DOM, ref ∷ REF | eff) (Array Attribute) (Exists Thunk)
  → DOM.Node
  → (a → VDom)
  → a
  → Eff (dom ∷ DOM, ref ∷ REF | eff) (a → Eff (dom ∷ DOM, ref ∷ REF | eff) Unit)
mkRenderQueue' spec parent render initialValue = do
  initMachine ← V.buildVDom spec (render initialValue)
  DOM.appendChild (V.extract initMachine) parent
  ref ← Ref.newRef initMachine
  pure \v → do
    machine ← Ref.readRef ref
    res ← V.step machine (render v)
    Ref.writeRef ref res

main :: ∀ eff. Eff (ref ∷ REF, dom ∷ DOM, dbmon ∷ DBMON, timer ∷ Timer.TIMER | eff) Unit
main = do
  win ← DOM.window
  doc ← DOM.document win
  bod ← DOM.querySelector "body" (DOM.htmlDocumentToParentNode doc)
  for_ (toMaybe bod) \body → do
    let spec = mkSpec (DOM.htmlDocumentToDocument doc)
    pushQueue ← mkRenderQueue' spec (DOM.elementToNode body) renderData initialState
    let
      loop = do
        newData ← getData
        timeout ← getTimeout
        pushQueue newData
        pingRenderRate
        void (Timer.setTimeout timeout loop)
    loop
