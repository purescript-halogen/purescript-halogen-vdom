module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Timer as Timer
import Data.Array as Array
import Data.Exists (Exists, mkExists, runExists)
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe)
import Data.Foldable (for_)
import Data.Function.Uncurried as Fn
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..), fst, snd)
import DOM (DOM)
import DOM.HTML (window) as DOM
import DOM.HTML.Types (htmlDocumentToDocument, htmlDocumentToParentNode) as DOM
import DOM.HTML.Window (document) as DOM
import DOM.Node.Element (setAttribute, removeAttribute) as DOM
import DOM.Node.Types (Document, Element, Node, elementToNode) as DOM
import DOM.Node.Node (appendChild) as DOM
import DOM.Node.ParentNode (querySelector) as DOM
import Halogen.VDom as V
import Halogen.VDom.Machine as M
import Unsafe.Coerce (unsafeCoerce)

infixr 1 Tuple as :=

type Attribute = Tuple String String

-- type VDom = V.VDom String (Array Attribute) Void

type VDom = V.VDom String (Array Attribute) (Exists Thunk)

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

elem ∷ ∀ k a w. String → a → Array (V.VDom k a w) → V.VDom k a w
elem n a = V.Elem (V.ElemSpec Nothing n a)

keyed ∷ ∀ k a w. String → a → Array (Tuple k (V.VDom k a w)) → V.VDom k a w
keyed n a = V.Keyed (V.ElemSpec Nothing n a)

text ∷ ∀ k a w. String → V.VDom k a w
text = V.Text

thunk ∷ ∀ a. (a → VDom) → a → VDom
thunk render val = V.Widget (mkExists (Thunk val render))

renderData ∷ State → VDom
renderData st =
  elem "div" []
    [ elem "table"
        [ "class" := "table table-striped latest data" ]
        -- [ keyed "tbody" [] (map (\db → Tuple db.dbname (thunk renderDatabase db)) st) ]
        [ elem "tbody" [] (map (thunk renderDatabase) st) ]
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

buildWidget ∷ ∀ eff. V.VDomSpec eff (Array Attribute) (Exists Thunk) → V.VDomMachine eff (Exists Thunk) DOM.Node
buildWidget spec = render
  where
  render = runExists \(Thunk a render) → do
    M.Step node m h ← V.buildVDom spec (render a)
    pure (M.Step node (Fn.runFn4 patch (unsafeCoerce a) node m h) h)

  patch = Fn.mkFn4 \a node step halt → runExists \(Thunk b render) →
    if Fn.runFn2 unsafeRefEq a b
      then pure (M.Step node (Fn.runFn4 patch a node step halt) halt)
      else do
        M.Step node' m h ← step (render b)
        pure (M.Step node (Fn.runFn4 patch (unsafeCoerce b) node' m h) h)

buildAttributes ∷ ∀ eff. DOM.Element → V.VDomMachine eff (Array Attribute) Unit
buildAttributes el = render
  where
  render as1 = do
    let
      as1' = Fn.runFn3 buildStrMap fst snd as1
      onAttr = Fn.mkFn2 \k v → DOM.setAttribute k v el
    Fn.runFn2 forInE as1' onAttr
    pure (M.Step unit (patch as1') done)

  patch as1 as2 = do
    let
      as2' = Fn.runFn3 buildStrMap fst snd as2
      onThese = Fn.mkFn3 \k a b → do
        if a /= b
          then DOM.setAttribute k b el
          else pure unit
      onThis = Fn.mkFn2 \k _ → DOM.removeAttribute k el
      onThat = Fn.mkFn2 \k a → DOM.setAttribute k a el
    Fn.runFn5 diffKeysE onThese onThis onThat as1 as2'
    pure (M.Step unit (patch as2') done)

  done = pure unit

mkSpec ∷ ∀ eff. DOM.Document → V.VDomSpec eff (Array Attribute) (Exists Thunk)
mkSpec document = V.VDomSpec
  { buildWidget
  , buildAttributes
  , document
  }

foreign import data DBMON ∷ !

foreign import getData ∷ ∀ eff. Eff (dbmon ∷ DBMON | eff) State

foreign import getTimeout ∷ ∀ eff. Eff (dbmon ∷ DBMON | eff) Int

foreign import pingRenderRate ∷ ∀ eff. Eff (dbmon ∷ DBMON | eff) Unit

foreign import forInE
  ∷ ∀ eff a b
  . Fn.Fn2
      (StrMap.StrMap a)
      (Fn.Fn2 String a (Eff eff b))
      (Eff eff Unit)

foreign import buildStrMap
  ∷ ∀ a b
  . Fn.Fn3
      (a → String)
      (a → b)
      (Array a)
      (StrMap.StrMap b)

foreign import diffKeysE
  ∷ ∀ eff a b c
  . Fn.Fn5
      (Fn.Fn3 String a b (Eff eff c))
      (Fn.Fn2 String a (Eff eff c))
      (Fn.Fn2 String b (Eff eff c))
      (StrMap.StrMap a)
      (StrMap.StrMap b)
      (Eff eff Unit)

foreign import unsafeRefEq
  ∷ ∀ a b
  . Fn.Fn2 a b Boolean

main :: ∀ eff. Eff (dom ∷ DOM, dbmon ∷ DBMON, timer ∷ Timer.TIMER | eff) Unit
main = do
  win ← DOM.window
  doc ← DOM.document win
  bod ← DOM.querySelector "body" (DOM.htmlDocumentToParentNode doc)
  for_ (toMaybe bod) \body → do
    let
      spec = mkSpec (DOM.htmlDocumentToDocument doc)
      loopTest rs n ms machine = do
        newData  ← getData
        machine' ← M.step machine (renderData (if rs > 0 then Array.take rs newData else newData))
        when (n > 0) do
          void $ Timer.setTimeout ms (loopTest rs (n - 1) ms machine')
        pure unit
      loopMain machine = do
        newData  ← getData
        timeout  ← getTimeout
        machine' ← M.step machine (renderData newData)
        pingRenderRate
        Timer.setTimeout timeout (loopMain machine')
        pure unit
    machine ← V.buildVDom spec (renderData initialState)
    DOM.appendChild (M.extract machine) (DOM.elementToNode body)
    -- loopTest 0 100 13 machine
    loopMain machine
