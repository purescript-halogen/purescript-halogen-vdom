module Test.Hydration where

import Prelude

import Control.Alternative (void)
import Data.Foldable (for_, traverse_)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Newtype (un, wrap)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Exception (error, throwException)
import Effect.Ref as Ref
import Effect.Timer as Timer
import Effect.Uncurried as EFn
import Halogen.VDom as V
import Halogen.VDom.DOM.Prop (Prop)
import Halogen.VDom.Thunk (Thunk)
import Halogen.VDom.Util (addEventListener) as Util
import Test.TestVdom (VDom(..), elem, keyed, mkSpec, text, thunk, (:=))
import Web.DOM.Element (Element)
import Web.DOM.Element (toNode) as DOM
import Web.DOM.Node (Node, appendChild) as DOM
import Web.DOM.ParentNode (ParentNode)
import Web.DOM.ParentNode (querySelector, QuerySelector(..)) as DOM
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (toDocument, toParentNode) as DOM
import Web.HTML.Window (document) as DOM
import Web.Event.EventTarget (eventListener, EventListener) as DOM

type State = Array { classes :: String, text :: String }

initialState ∷ State
initialState =
  [ { classes: "label1", text: "test label 1" }
  , { classes: "label2", text: "test label 2" }
  ]

state2 ∷ State
state2 =
  [ { classes: "label2", text: "test label 1.1" }
  , { classes: "label1", text: "test label 2.1" }
  ]

renderData ∷ State → VDom Void
renderData st =
  elem "div" [ "className" := "component" ] (st <#> renderElement)
  where
    renderElement elementState =
      elem "div"
      [ "className" := elementState.classes ]
      [ text elementState.text ]

findRequiredElement :: String -> ParentNode -> Effect Element
findRequiredElement selector parentNode =
  DOM.querySelector (DOM.QuerySelector selector) parentNode
    >>= maybe (throwException (error $ selector <> " not found")) pure

main ∷ Effect Unit
main = do
  win ← DOM.window
  doc ← DOM.document win
  appDiv ← findRequiredElement "#app" (DOM.toParentNode doc)
  updateStateButton ← findRequiredElement "#update-state-button" (DOM.toParentNode doc)

  let
    spec = mkSpec (DOM.toDocument doc)
    initialValue = initialState
    appDivNode = DOM.toNode appDiv
    render = renderData
  machine ← EFn.runEffectFn1 (V.buildVDom spec) (un VDom (render initialValue))
  void $ DOM.appendChild (V.extract machine) appDivNode

  listener ← DOM.eventListener \_ev →
    void $ EFn.runEffectFn2 V.step machine (un VDom (render state2))

  EFn.runEffectFn3 Util.addEventListener "click" listener updateStateButton
