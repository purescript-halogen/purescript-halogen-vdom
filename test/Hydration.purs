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
import Web.DOM.Element (toParentNode) as DOM.Element
import Web.DOM.Node (Node, appendChild) as DOM
import Web.DOM.ParentNode (ParentNode)
import Web.DOM.ParentNode (firstElementChild) as DOM.ParentNode
import Web.DOM.ParentNode (querySelector, QuerySelector(..)) as DOM
import Web.Event.EventTarget (eventListener, EventListener) as DOM
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
  keyed "div" [ "className" := "component" ] (map (\state → Tuple state.key (renderElement state)) stateArray)
  where
    renderElement elementState =
      elem "div"
      [ "className" := elementState.classes ]
      [ text elementState.text ]

findRequiredElement ∷ String → ParentNode → Effect Element
findRequiredElement selector parentNode =
  DOM.querySelector (DOM.QuerySelector selector) parentNode
    >>= maybe (throwException (error $ selector <> " not found")) pure

main ∷ Effect Unit
main = do
  win ← DOM.window
  doc ← DOM.document win
  appDiv ← findRequiredElement "#app" (DOM.toParentNode doc)

  rootElement ← (appDiv # DOM.Element.toParentNode # DOM.ParentNode.firstElementChild)
    >>= maybe (throwException (error $ "rootElement not found")) pure

  updateStateButton ← findRequiredElement "#update-state-button" (DOM.toParentNode doc)

  let
    spec = mkSpec (DOM.toDocument doc)
    initialValue = initialState
    render = renderData
    initialVdom = un VDom (render initialValue)
  machine ← EFn.runEffectFn1 (V.hydrateVDom spec rootElement) initialVdom

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

-- | Having `<input type="text" value="foo1">`
-- | If do `$0.required = true` in chrome.
-- |   THE html `<input type="text" value="foo1" required>`
-- |   THE $0.attributes = { required: "true" }
-- |
-- | Having `<input type="text" value="foo1" required>`
-- | If do `$0.required = false` in chrome.
-- |   THE html `<input type="text" value="foo1">`
-- |   THE $0.attributes = {}
-- |
-- | thus, we should check prop is set and remove it from extraAttributeNames

-- | Having `<div id="1" data-foo="foo" data-bar="bar" data-baz-bak="baz-bak" data-someint="1"></div>`
-- |   The `$0.attributes`
-- |     `NamedNodeMap { id: "1", data-foo: "foo", data-bar: "bar", data-baz-bak: "baz-bak", someint: "1" }`
-- |   The `$0.dataset`
-- |     `DOMStringMap { foo: "foo", bar: "bar", bazBak: "baz-bak", someint: "1" }`
-- |
-- | Also, react doesnt support dataset property. Proof:
-- |   server = `<div id="1" data-foo="foo">`
-- |   client = `<div id="1" dataset={{ foo : "foo" }}></div>`
-- |   errorMessage = `Prop `dataset` did not match. Server: "null" Client: "[object Object]"`
-- |
-- | thus, react does support `data-***` attributes, but doesn't support `dataset` property, so why bother with supporting `dataset` property?
-- |
-- | If we wont ignore dataset, then we sould implment something like
-- |
-- | data PropValue = PropValue_String String | PropValue_Int Int | ... | PropValue_Dataset (Object String)
-- | removePropFromExtraAttributeNames ∷ PropName → PropValue → Set → Set
-- | removePropFromExtraAttributeNames propName propValue set =
-- |   if propName == "dataset"
-- |     then forEach propValue
-- |       (\key _val → do
-- |         remove ("data-" <> camelCaseToKebabCase key) set
-- |       )
-- |     else do
-- |        remove (camelCaseToKebabCase key) set

-- `$0.attributes` are:
-- <input type="text" value="foo1" required> is { required: "" }
-- <input type="text" value="foo1" required="false"> is { required: "false" }
-- <td colspan="1"> is { colspan: "1" }, but prop is colSpan = 1
-- <div foo:data-foo="1"> is { "foo:data-foo": "1" }
