module Test.Hydration where

import Prelude

import Effect.Exception (error, throwException)
import Data.Either (either)
import Data.Newtype (un)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Uncurried as EFn
import Halogen.VDom as V
import Halogen.VDom.Finders (findElementFirstChild)
import Web.DOM.ParentNode (ParentNode)
import Web.DOM.Element as DOM.Element
import Web.DOM (Element)
import Web.DOM.ParentNode (querySelector, QuerySelector(..)) as DOM
import Data.Maybe (maybe)
import Halogen.VDom.Util (addEventListener) as Util
import Test.TestVdom (VDom(..), elem, keyed, mkSpec, text, thunk, (:=))
import Web.Event.EventTarget (eventListener) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toDocument, toParentNode) as DOM
import Web.HTML.Window (document) as DOM

type State = Array { classes ∷ String, text ∷ String, key ∷ String }

initialState ∷ State
initialState =
  [ { classes: "label1", text: "test label 1", key: "1" }
  , { classes: "label2", text: "test label 2", key: "2" }
  ]

state2 ∷ State
state2 =
  [ { classes: "label2", text: "test label 1.1", key: "1" }
  , { classes: "label1", text: "test label 2.1", key: "2" }
  ]

renderData ∷ State → VDom Void
renderData stateArray =
  -- | keyed "div" [ "className" := "component" ] (stateArray <#> renderElement)
  -- | keyed "div" [ "className" := "component" ] (map (\state → Tuple state.key (renderElement state)) stateArray)
  keyed "div" [ "className" := "component" ] (map (\state → Tuple state.key (thunk renderElement state)) stateArray)
  where
    renderElement elementState =
      elem "div"
      [ "className" := elementState.classes ]
      [ text elementState.text ]

findRequiredElement ∷ String → ParentNode → Effect Element
findRequiredElement selector parentNode = do
  maybeElement <- DOM.querySelector (DOM.QuerySelector selector) parentNode
  maybe (throwException $ error $ selector <> " not found") pure maybeElement

main ∷ Effect Unit
main = do
  win ← DOM.window
  doc ← DOM.document win

  appDiv ← findRequiredElement "#app" (DOM.toParentNode doc)

  rootElement ← findElementFirstChild appDiv >>= either (throwException <<< error) pure

  updateStateButton ← findRequiredElement "#update-state-button" (DOM.toParentNode doc)

  let
    spec = mkSpec (DOM.toDocument doc)
    initialValue = initialState
    render = renderData
    initialVdom = un VDom (render initialValue)
  machine ← EFn.runEffectFn1 (V.hydrateVDom spec (DOM.Element.toNode rootElement)) initialVdom

  listener ← DOM.eventListener \_ev →
    void $ EFn.runEffectFn2 V.step machine (un VDom (render state2))

  EFn.runEffectFn3 Util.addEventListener "click" listener updateStateButton

tests ∷ Array { client ∷ String , errorMessage ∷ String , server ∷ String , title ∷ String }
tests =
  -- | [ { title: "Attribute → renders"
  [ { title: "Attribute → missing prop"
    , server: """
    <div class="label1">test label 1</div>
    """
    , client: """
    <div className="label1" foo="bar">test label 1</div>
    """
    , errorMessage: """
    Warning: Prop `%s` did not match. Server: %s Client: %s%s
    """
    }
  , { title: "Attribute → extra prop"
    , server: """
    <div class="label1" foo="bar">test label 1</div>
    """
    , client: """
    <div className="label1">test label 1</div>
    """
    , errorMessage: """
    Warning: Extra attributes from the server: %s%s
    """
    }
  , { title: "Attribute → did not match"
    , server: """
    <div class="label1" foo="bar">test label 1</div>
    """
    , client: """
    <div className="label1" foo="baz">test label 1</div>
    """
    , errorMessage: """
    Warning: Prop `%s` did not match. Server: %s Client: %s%s
    """
    }
  -- | , { title: "Prop → boolean → "
  , { title: "Prop → controlled element → renders"
    , server: """
    <input type="text" value="foo">
    """
    , client: """
    <input type="text" value="foo" onChange={console.log} />
    """
    , errorMessage: """
    """
    }
  , { title: "Prop → controlled element → did not match → renders"
    , server: """
    <input type="text" value="foo">
    """
    , client: """
    <input type="text" value="foo" onChange={console.log} />
    """
    , errorMessage: """
    """
    }
  ]
