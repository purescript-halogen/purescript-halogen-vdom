module Test.Main where

import Prelude

import Data.Exists (Exists, mkExists)
import Data.Foldable (for_, traverse_)
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (wrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Timer as Timer
import Effect.Uncurried as EFn
import Halogen.VDom as V
import Halogen.VDom.DOM.Prop (Prop(..), propFromString, buildProp)
import Halogen.VDom.Util (refEq)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (Document) as DOM
import Web.DOM.Element (toNode) as DOM
import Web.DOM.Node (Node, appendChild) as DOM
import Web.DOM.ParentNode (querySelector) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toDocument, toParentNode) as DOM
import Web.HTML.Window (document) as DOM

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
elem n a = V.Elem Nothing (V.ElemName n) a

keyed ∷ ∀ a w. String → a → Array (Tuple String (V.VDom a w)) → V.VDom a w
keyed n a = V.Keyed Nothing (V.ElemName n) a

text ∷ ∀ a w. String → V.VDom a w
text = V.Text

thunk ∷ ∀ a. (a → VDom) → a → VDom
thunk render val = V.Widget (mkExists (Thunk val render))

renderData ∷ State → VDom
renderData st =
  elem "div" []
    [ elem "table"
        [ "className" := "table table-striped latest data" ]
        [ keyed "tbody" [] (map (\db → Tuple db.dbname (thunk renderDatabase db)) st) ]
        -- [ keyed "tbody" [] (map (\db → Tuple db.dbname (renderDatabase db)) st) ]
        -- [ elem "tbody" [] (map (thunk renderDatabase) st) ]
        -- [ elem "tbody" [] (map renderDatabase st) ]
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

type WidgetState a w =
  { t :: Exists Thunk
  , step :: V.Step a w
  }

buildWidget
  ∷ V.VDomSpec (Array (Prop Void)) (Exists Thunk)
  → V.Machine (Exists Thunk) DOM.Node
buildWidget spec = render
  where
  render = EFn.mkEffectFn1 \t → case unsafeCoerce t of
    Thunk a render' → do
      step ← EFn.runEffectFn1 (V.buildVDom spec) (render' a)
      let state = { t, step }
      pure (V.mkStep (V.Step (V.extract step) state patch done))

  patch = EFn.mkEffectFn2 \state t →
    case unsafeCoerce state.t, unsafeCoerce t of
      Thunk a render1, Thunk b render2 →
        if Fn.runFn2 refEq a b && Fn.runFn2 refEq render1 render2
          then
            pure (V.mkStep (V.Step (V.extract state.step) state patch done))
          else do
            step ← EFn.runEffectFn2 V.step state.step (render2 b)
            let nextState = { t, step }
            pure (V.mkStep (V.Step (V.extract step) nextState patch done))

  done = EFn.mkEffectFn1 \state → do
    EFn.runEffectFn1 V.halt state.step

mkSpec
  ∷ DOM.Document
  → V.VDomSpec (Array (Prop Void)) (Exists Thunk)
mkSpec document = V.VDomSpec
  { buildWidget
  , buildAttributes: buildProp (const (pure unit))
  , document
  }

foreign import getData ∷ Effect State

foreign import getTimeout ∷ Effect Int

foreign import pingRenderRate ∷ Effect Unit

foreign import requestAnimationFrame ∷ Effect Unit → Effect Unit

mkRenderQueue
  ∷ ∀ a
  . V.VDomSpec (Array (Prop Void)) (Exists Thunk)
  → DOM.Node
  → (a → VDom)
  → a
  → Effect (a → Effect Unit)
mkRenderQueue spec parent render initialValue = do
  initMachine ← EFn.runEffectFn1 (V.buildVDom spec) (render initialValue)
  _ ← DOM.appendChild (V.extract initMachine) parent
  ref ← Ref.new initMachine
  val ← Ref.new Nothing
  pure \a → do
    v ← Ref.read val
    Ref.write (Just a) val
    when (isNothing v) $ requestAnimationFrame do
      machine ← Ref.read ref
      Ref.read val >>= traverse_ \v' → do
        res ← EFn.runEffectFn2 V.step machine (render v')
        Ref.write res ref
        Ref.write Nothing val

mkRenderQueue'
  ∷ ∀ a
  . V.VDomSpec (Array (Prop Void)) (Exists Thunk)
  → DOM.Node
  → (a → VDom)
  → a
  → Effect (a → Effect Unit)
mkRenderQueue' spec parent render initialValue = do
  initMachine ← EFn.runEffectFn1 (V.buildVDom spec) (render initialValue)
  _ ← DOM.appendChild (V.extract initMachine) parent
  ref ← Ref.new initMachine
  pure \v → do
    machine ← Ref.read ref
    res ← EFn.runEffectFn2 V.step machine (render v)
    Ref.write res ref

main ∷ Effect Unit
main = do
  win ← DOM.window
  doc ← DOM.document win
  bod ← DOM.querySelector (wrap "body") (DOM.toParentNode doc)
  for_ bod \body → do
    let spec = mkSpec (DOM.toDocument doc)
    pushQueue ← mkRenderQueue' spec (DOM.toNode body) renderData initialState
    let
      loop = do
        newData ← getData
        timeout ← getTimeout
        pushQueue newData
        pingRenderRate
        void (Timer.setTimeout timeout loop)
    loop
