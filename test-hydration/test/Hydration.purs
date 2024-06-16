module Test.Hydration where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (launchAff_, throwError)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (Error, error)
import Test.Hydration.Basic ((.=), (:=))
import Test.Hydration.Basic as Basic
import Test.Hydration.Driver as Driver
import Test.Hydration.JSDOM as JSDOM
import Test.Hydration.Render as Render
import Test.Spec (describe, it)
import Test.Spec.Assertions (expectError, shouldReturn)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (runSpec)
import Web.DOM (Document, Node) as DOM
import Web.DOM.Element as Element
import Web.DOM.ParentNode (QuerySelector(..), querySelector) as DOM
import Web.HTML (HTMLDocument) as DOM
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window as Window

findInitialNode ∷ DOM.HTMLDocument → Effect DOM.Node
findInitialNode htmlDocument = do
  mRootElement ← DOM.querySelector (DOM.QuerySelector "#root")
    (HTMLDocument.toParentNode htmlDocument)
  case mRootElement of
    Just rootElement →
      pure $ Element.toNode rootElement
    Nothing →
      throwError $ error "Cannot find #root in document."

renderHydrate
  ∷ ∀ m a
  . MonadEffect m
  ⇒ MonadThrow Error m
  ⇒ (Boolean → Basic.VDom a)
  → m Unit
renderHydrate renderFn = liftEffect do
  let
    serverHTML ∷ String
    serverHTML = "<!DOCTYPE html><html><head></head><body>"
      <> Render.render (renderFn false)
      <> "</body></html>"

  jsdom ← JSDOM.make serverHTML
  window ← JSDOM.window jsdom
  htmlDocument ← Window.document window
  initialNode ← findInitialNode htmlDocument

  let
    document ∷ DOM.Document
    document = HTMLDocument.toDocument htmlDocument

    driverSpec ∷ Driver.DriverSpec Boolean a
    driverSpec = { initialState: true, render: renderFn }

  void $ Driver.hydrateUI document initialNode driverSpec

main ∷ Effect Unit
main = launchAff_ $ runSpec [ consoleReporter ] do
  describe "Hydration" do
    it "works" do
      let
        general ∷ Boolean → Basic.VDom Void
        general _ =
          Basic.elem "div" [ "className" .= "container", "id" := "root" ]
            [ Basic.elem "label" [ "htmlFor" .= "username" ]
                [ Basic.text "Username" ]
            , Basic.elem "input" [ "id" := "input" ] []
            , Basic.elem "a" [ "href" := "index" ] [ Basic.text "Inbox" ]
            , Basic.keyed "div" []
                [ "0" /\ Basic.elem "span" [] [ Basic.text "0" ]
                , "1" /\ Basic.elem "span" [] [ Basic.text "1" ]
                ]
            ]
      renderHydrate general `shouldReturn` unit

    it "fails on mismatched properties" do
      let
        mismatch ∷ Boolean → Basic.VDom Void
        mismatch isClient =
          Basic.elem "div" [ "className" .= show isClient, "id" := "root" ] []
      expectError $ renderHydrate mismatch

    it "fails on mismatched attributes" do
      let
        mismatch ∷ Boolean → Basic.VDom Void
        mismatch isClient =
          Basic.elem "div" [ "style" := show isClient, "id" := "root" ] []
      expectError $ renderHydrate mismatch

    it "fails on mismatched elements" do
      let
        mismatch ∷ Boolean → Basic.VDom Void
        mismatch isClient =
          Basic.elem (if isClient then "div" else "span") [ "id" := "root" ] []
      expectError $ renderHydrate mismatch

    it "fails on mismatched text nodes" do
      let
        mismatch ∷ Boolean → Basic.VDom Void
        mismatch isClient =
          Basic.elem "div" [ "id" := "root" ] [ Basic.text $ show isClient ]
      expectError $ renderHydrate mismatch

    it "fails on mismatched children" do
      let
        mismatch ∷ Boolean → Basic.VDom Void
        mismatch isClient =
          Basic.elem "div" [ "id" := "root" ] $
            if isClient then []
            else [ Basic.elem "span" [] [ Basic.text "Server!" ] ]
      expectError $ renderHydrate mismatch
