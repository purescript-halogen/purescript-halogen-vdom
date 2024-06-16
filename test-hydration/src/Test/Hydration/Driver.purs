module Test.Hydration.Driver where

import Prelude

import Effect (Effect)
import Effect.Uncurried (runEffectFn1, runEffectFn2)
import Halogen.VDom as V
import Safe.Coerce (coerce)
import Test.Hydration.Basic as Basic
import Web.DOM as DOM

type DriverSpec s a =
  { initialState ∷ s
  , render ∷ s → Basic.VDom a
  }

type DriverStep a = V.Step (Basic.Core a) DOM.Node

type DriverPackage s a =
  { machine ∷ DriverStep a
  , render ∷ DriverStep a → s → Effect (DriverStep a)
  }

hydrateUI
  ∷ ∀ s a. DOM.Document → DOM.Node → DriverSpec s a → Effect (DriverPackage s a)
hydrateUI document initialNode { initialState, render } = do
  let
    hydrationSpec ∷ Basic.VDomHydrationSpec a
    hydrationSpec = Basic.mkSpecWithHydration document

    renderCore ∷ s → Basic.Core a
    renderCore = coerce render

    initialRender ∷ Basic.Core a
    initialRender = renderCore initialState

    renderStep ∷ DriverStep a → s → Effect (DriverStep a)
    renderStep machine core = runEffectFn2 V.step machine $ renderCore core

  initialMachine ← runEffectFn1 (V.hydrateVDom hydrationSpec initialNode)
    initialRender
  pure { machine: initialMachine, render: renderStep }
