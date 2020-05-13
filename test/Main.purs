module Test.Main where

import Prelude

import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..), isNothing)
import Data.Newtype (un, wrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Timer as Timer
import Effect.Uncurried as EFn
import Halogen.VDom as V
import Halogen.VDom.DOM.Prop (Prop)
import Halogen.VDom.Thunk (Thunk)
import Web.DOM.Element (toNode) as DOM
import Web.DOM.Node (Node, appendChild) as DOM
import Web.DOM.ParentNode (querySelector) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toDocument, toParentNode) as DOM
import Web.HTML.Window (document) as DOM
import Test.TestVdom (VDom(..), elem, keyed, mkSpec, text, thunk, (:=))

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

renderData ∷ State → VDom Void
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

foreign import getData ∷ Effect State

foreign import getTimeout ∷ Effect Int

foreign import pingRenderRate ∷ Effect Unit

foreign import requestAnimationFrame ∷ Effect Unit → Effect Unit

mkRenderQueue
  ∷ ∀ a
  . V.VDomSpec (Array (Prop Void)) (Thunk VDom Void)
  → DOM.Node
  → (a → VDom Void)
  → a
  → Effect (a → Effect Unit)
mkRenderQueue spec parent render initialValue = do
  initMachine ← EFn.runEffectFn1 (V.buildVDom spec) (un VDom (render initialValue))
  _ ← DOM.appendChild (V.extract initMachine) parent
  ref ← Ref.new initMachine
  val ← Ref.new Nothing
  pure \a → do
    v ← Ref.read val
    Ref.write (Just a) val
    when (isNothing v) $ requestAnimationFrame do
      machine ← Ref.read ref
      Ref.read val >>= traverse_ \v' → do
        res ← EFn.runEffectFn2 V.step machine (un VDom (render v'))
        Ref.write res ref
        Ref.write Nothing val

mkRenderQueue'
  ∷ ∀ a
  . V.VDomSpec (Array (Prop Void)) (Thunk VDom Void)
  → DOM.Node
  → (a → VDom Void)
  → a
  → Effect (a → Effect Unit)
mkRenderQueue' spec parent render initialValue = do
  initMachine ← EFn.runEffectFn1 (V.buildVDom spec) (un VDom (render initialValue))
  _ ← DOM.appendChild (V.extract initMachine) parent
  ref ← Ref.new initMachine
  pure \v → do
    machine ← Ref.read ref
    res ← EFn.runEffectFn2 V.step machine (un VDom (render v))
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
