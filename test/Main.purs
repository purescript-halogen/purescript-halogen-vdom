module Test.Main where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)
import Control.Monad.Eff.Ref (REF)
import Control.Monad.Eff.Ref as Ref
import Control.Monad.Eff.Timer as Timer
import Data.Exists (Exists, mkExists, runExists)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (wrap)
import Data.Foldable (for_, traverse_)
import Data.Function.Uncurried as Fn
import Data.Tuple (Tuple)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument, htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.Types (Document, Node, elementToNode) as DOM
import DOM.Node.Node (appendChild) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import Halogen.VDom as V
import Halogen.VDom.Util (refEq)
import Halogen.VDom.DOM.Prop (Prop(..), propFromString, buildProp)
import Unsafe.Coerce (unsafeCoerce)

infixr 1 prop as :=

prop ∷ ∀ a. String → String → Prop a
prop key val = Property key (propFromString val)

type VDom = V.VDom (Array (Prop Void)) (Exists Thunk)

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
        [ "className" := "table table-striped latest data" ]
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
          [ "className" := "dbname" ]
          [ text db.dbname ]
      , elem "td"
          [ "className" := "query-count" ]
          [ elem "span"
              [ "className" := db.lastSample.countClassName ]
              [ text (show db.lastSample.nbQueries) ]
          ]
      ] <> map renderQuery db.lastSample.topFiveQueries)

  renderQuery q =
    elem "td"
      [ "className" := "Query" <> q.elapsedClassName ]
      [ text q.formatElapsed
      , elem "div"
          [ "className" := "popover left" ]
          [ elem "div"
              [ "className" := "popover-content" ]
              [ text q.query ]
          , elem "div"
              [ "className" := "arrow" ]
              []
          ]
      ]

buildWidget
  ∷ ∀ eff
  . V.VDomSpec (dom ∷ DOM, ref ∷ REF | eff) (Array (Prop Void)) (Exists Thunk)
  → V.VDomMachine (dom ∷ DOM, ref ∷ REF | eff) (Exists Thunk) DOM.Node
buildWidget spec = render
  where
  render = runExists \(Thunk a render') → do
    V.Step node m h ← V.buildVDom spec (render' a)
    pure (V.Step node (Fn.runFn4 patch (unsafeCoerce a) node m h) h)

  patch = Fn.mkFn4 \a node step halt → runExists \(Thunk b render') →
    if Fn.runFn2 refEq a b
      then pure (V.Step node (Fn.runFn4 patch a node step halt) halt)
      else do
        V.Step node' m h ← step (render' b)
        pure (V.Step node' (Fn.runFn4 patch (unsafeCoerce b) node' m h) h)

mkSpec
  ∷ ∀ eff
  . DOM.Document
  → V.VDomSpec (dom ∷ DOM, ref ∷ REF | eff) (Array (Prop Void)) (Exists Thunk)
mkSpec document = V.VDomSpec
  { buildWidget
  , buildAttributes: buildProp (const (pure unit))
  , document
  }

foreign import data DBMON ∷ Effect

foreign import getData ∷ ∀ eff. Eff (dbmon ∷ DBMON | eff) State

foreign import getTimeout ∷ ∀ eff. Eff (dbmon ∷ DBMON | eff) Int

foreign import pingRenderRate ∷ ∀ eff. Eff (dbmon ∷ DBMON | eff) Unit

foreign import requestAnimationFrame ∷ ∀ eff. Eff (dom ∷ DOM | eff) Unit → Eff (dom ∷ DOM | eff) Unit

mkRenderQueue
  ∷ ∀ eff a
  . V.VDomSpec (dom ∷ DOM, ref ∷ REF | eff) (Array (Prop Void)) (Exists Thunk)
  → DOM.Node
  → (a → VDom)
  → a
  → Eff (dom ∷ DOM, ref ∷ REF | eff) (a → Eff (dom ∷ DOM, ref ∷ REF | eff) Unit)
mkRenderQueue spec parent render initialValue = do
  initMachine ← V.buildVDom spec (render initialValue)
  _ ← DOM.appendChild (V.extract initMachine) parent
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
  . V.VDomSpec (dom ∷ DOM, ref ∷ REF | eff) (Array (Prop Void)) (Exists Thunk)
  → DOM.Node
  → (a → VDom)
  → a
  → Eff (dom ∷ DOM, ref ∷ REF | eff) (a → Eff (dom ∷ DOM, ref ∷ REF | eff) Unit)
mkRenderQueue' spec parent render initialValue = do
  initMachine ← V.buildVDom spec (render initialValue)
  _ ← DOM.appendChild (V.extract initMachine) parent
  ref ← Ref.newRef initMachine
  pure \v → do
    machine ← Ref.readRef ref
    res ← V.step machine (render v)
    Ref.writeRef ref res

main :: ∀ eff. Eff (ref ∷ REF, dom ∷ DOM, dbmon ∷ DBMON, timer ∷ Timer.TIMER | eff) Unit
main = do
  win ← DOM.window
  doc ← DOM.document win
  bod ← DOM.querySelector (wrap "body") (DOM.htmlDocumentToParentNode doc)
  for_ bod \body → do
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
