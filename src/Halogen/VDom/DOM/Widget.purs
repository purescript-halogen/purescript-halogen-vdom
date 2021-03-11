module Halogen.VDom.DOM.Widget where

import Effect.Uncurried as EFn
import Halogen.VDom.DOM.Types (VDomBuilder, VDomMachine, VDomSpec(..), VDomSpecWithHydration(..), VDomStep, VDomHydrator)
import Halogen.VDom.Machine (Step, Step'(..), halt, mkStep, step, unStep)
import Halogen.VDom.Types (VDom(..), runGraft)
import Prelude (Unit, bind, discard, pure, (#), ($))
import Web.DOM.Node (Node) as DOM

type WidgetState a w =
  { build ∷ VDomMachine a w
  , widget ∷ Step w DOM.Node
  }

hydrateWidget ∷ ∀ a w. VDomHydrator w a w
hydrateWidget = EFn.mkEffectFn5 \elem (VDomSpecWithHydration spec) _hydrate build w → do
  res ← EFn.runEffectFn1 (spec.hydrateWidget (VDomSpecWithHydration spec) elem) w
  let
    res' :: Step (VDom a w) DOM.Node
    res' = res # unStep \(Step n s k1 k2) →
      mkStep $ Step n { build, widget: res } patchWidget haltWidget
  pure res'

buildWidget ∷ ∀ a w. VDomBuilder w a w
buildWidget = EFn.mkEffectFn3 \(VDomSpec spec) build w → do
  res ← EFn.runEffectFn1 (spec.buildWidget (VDomSpec spec)) w
  let
    res' :: Step (VDom a w) DOM.Node
    res' = res # unStep \(Step n s k1 k2) →
      mkStep $ Step n { build, widget: res } patchWidget haltWidget
  pure res'

patchWidget ∷ ∀ a w. EFn.EffectFn2 (WidgetState a w) (VDom a w) (VDomStep a w)
patchWidget = EFn.mkEffectFn2 \state vdom → do
  let { build, widget } = state
  case vdom of
    Grafted g →
      EFn.runEffectFn2 patchWidget state (runGraft g)
    Widget w → do
      res ← EFn.runEffectFn2 step widget w
      let
        res' = res # unStep \(Step n s k1 k2) →
          mkStep $ Step n { build, widget: res } patchWidget haltWidget
      pure res'
    _ → do
      EFn.runEffectFn1 haltWidget state
      EFn.runEffectFn1 build vdom

haltWidget ∷ forall a w. EFn.EffectFn1 (WidgetState a w) Unit
haltWidget = EFn.mkEffectFn1 \{ widget } → do
  EFn.runEffectFn1 halt widget
