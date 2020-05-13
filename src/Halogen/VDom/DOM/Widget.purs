module Halogen.VDom.DOM.Widget where

import Prelude

import Data.Array as Array
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Tuple (Tuple(..), fst)
import Effect.Uncurried as EFn
import Foreign.Object as Object
import Halogen.VDom.Machine (Machine, Step, Step'(..), extract, halt, mkStep, step, unStep)
import Halogen.VDom.Machine as Machine
import Halogen.VDom.Types (ElemName(..), Namespace(..), VDom(..), runGraft)
import Halogen.VDom.Util as Util
import Web.DOM.Document (Document) as DOM
import Web.DOM.Element (Element) as DOM
import Web.DOM.Element as DOMElement
import Web.DOM.Node (Node) as DOM
import Halogen.VDom.DOM.Types

type WidgetState a w =
  { build ∷ VDomMachine a w
  , widget ∷ Step w DOM.Node
  }

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
