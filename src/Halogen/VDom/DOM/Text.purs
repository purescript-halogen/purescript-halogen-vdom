module Halogen.VDom.DOM.Text where

import Halogen.VDom.DOM.Types
import Halogen.VDom.DOM.Utils
import Prelude

import Data.Array as Array
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Effect.Uncurried as EFn
import Foreign.Object as Object
import Halogen.VDom.Machine (Machine, Step, Step'(..), extract, halt, mkStep, step, unStep)
import Halogen.VDom.Machine as Machine
import Halogen.VDom.Types (ElemName(..), Namespace(..), VDom(..), runGraft)
import Halogen.VDom.Util as Util
import Web.DOM.Document (Document) as DOM
import Web.DOM.Element (Element) as DOM
import Web.DOM.Element (toNode) as DOM.Element
import Web.DOM.Element as DOM.Element
import Web.DOM.Node (Node) as DOM
import Web.DOM.Node (textContent)

type TextState a w =
  { build ∷ VDomMachine a w
  , node ∷ DOM.Node
  , value ∷ String
  }

buildText ∷ ∀ a w. VDomBuilder String a w
buildText = EFn.mkEffectFn3 \(VDomSpec spec) build s → do
  node ← EFn.runEffectFn2 Util.createTextNode s spec.document
  let (state :: TextState a w) = { build, node, value: s }
  pure $ mkStep $ Step node state patchText haltText

-- TODO: rename this to `hydrateTextDebug` and add another function `hydrateText` but without checks?
hydrateText ∷ ∀ a w. VDomHydrator String a w
hydrateText = EFn.mkEffectFn5 \currentElement (VDomSpec spec) _hydrate build s → do
  let
    currentNode :: DOM.Node
    currentNode = DOM.Element.toNode currentElement

  checkIsTextNode currentElement
  checkTextContentIsEqTo s currentElement
  let (state :: TextState a w) = { build, node: currentNode, value: s }
  pure $ mkStep $ Step currentNode state patchText haltText

patchText ∷ ∀ a w. EFn.EffectFn2 (TextState a w) (VDom a w) (VDomStep a w)
patchText = EFn.mkEffectFn2 \state newVdom → do
  let { build, node, value: value1 } = state
  case newVdom of
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
      EFn.runEffectFn1 build newVdom

haltText ∷ ∀ a w. EFn.EffectFn1 (TextState a w) Unit
haltText = EFn.mkEffectFn1 \{ node } → do
  parent ← EFn.runEffectFn1 Util.parentNode node
  EFn.runEffectFn2 Util.removeChild node parent
