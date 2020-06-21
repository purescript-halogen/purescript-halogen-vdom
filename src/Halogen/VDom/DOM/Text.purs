module Halogen.VDom.DOM.Text where

import Effect.Uncurried as EFn
import Halogen.VDom.DOM.Checkers (checkIsTextNode, checkTextContentIsEqTo)
import Halogen.VDom.DOM.Types (VDomBuilder, VDomHydrator, VDomMachine, VDomSpec(..), VDomStep)
import Halogen.VDom.Machine (Step'(..), mkStep)
import Halogen.VDom.Types (VDom(..), runGraft)
import Halogen.VDom.Util (warnAny)
import Halogen.VDom.Util as Util
import Prelude (Unit, bind, discard, otherwise, pure, ($), (==))
import Web.DOM.Element as DOM.Element
import Web.DOM.Node (Node) as DOM

type TextState a w =
  { build ∷ VDomMachine a w
  , node ∷ DOM.Node
  , value ∷ String
  }

-- TODO: rename this to `hydrateTextDebug` and add another function `hydrateText` but without checks?
hydrateText ∷ ∀ a w. VDomHydrator String a w
hydrateText = EFn.mkEffectFn5 \currentNode (VDomSpec spec) _hydrate build s → do
  EFn.runEffectFn2 warnAny "hydrateText" { s }
  currentText <- checkIsTextNode currentNode
  checkTextContentIsEqTo s currentText
  let (state :: TextState a w) = { build, node: currentNode, value: s }
  pure $ mkStep $ Step currentNode state patchText haltText

buildText ∷ ∀ a w. VDomBuilder String a w
buildText = EFn.mkEffectFn3 \(VDomSpec spec) build s → do
  EFn.runEffectFn2 warnAny "buildText" { s }
  node ← EFn.runEffectFn2 Util.createTextNode s spec.document
  let (state :: TextState a w) = { build, node, value: s }
  pure $ mkStep $ Step node state patchText haltText

patchText ∷ ∀ a w. EFn.EffectFn2 (TextState a w) (VDom a w) (VDomStep a w)
patchText = EFn.mkEffectFn2 \state vdom → do
  EFn.runEffectFn2 warnAny "patchText" { state, vdom }
  let { build, node, value: value1 } = state
  case vdom of
    Grafted g →
      EFn.runEffectFn2 patchText state (runGraft g) -- Before there was a Text on this place. We call patchText instead of patch to be able to remove text
    Text value2
      | value1 == value2 →
          pure $ mkStep $ Step node state patchText haltText
      | otherwise → do
          let nextState = { build, node, value: value2 }
          EFn.runEffectFn2 Util.setTextContent value2 node
          pure $ mkStep $ Step node nextState patchText haltText
    _ → do
      EFn.runEffectFn1 haltText state
      EFn.runEffectFn1 build vdom

haltText ∷ ∀ a w. EFn.EffectFn1 (TextState a w) Unit
haltText = EFn.mkEffectFn1 \{ node } → do
  EFn.runEffectFn2 warnAny "haltText" { node }
  parent ← EFn.runEffectFn1 Util.parentNode node
  EFn.runEffectFn2 Util.removeChild node parent
