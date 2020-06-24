module Halogen.VDom.DOM.Types where

import Prelude

import Effect.Uncurried as EFn
import Halogen.VDom.Machine (Machine, Step)
import Halogen.VDom.Types (VDom)
import Web.DOM.Document (Document) as DOM
import Web.DOM.Element (Element) as DOM
import Web.DOM.Node (Node) as DOM

type VDomMachine a w = Machine (VDom a w) DOM.Node

type VDomStep a w = Step (VDom a w) DOM.Node

type VDomInit i a w = EFn.EffectFn1 i (VDomStep a w)

type VDomBuilder i a w = EFn.EffectFn3 (VDomSpec a w) (VDomMachine a w) i (VDomStep a w)

type VDomHydrator i a w
  = EFn.EffectFn5
  DOM.Node -- current element
  (VDomSpecWithHydration a w)
  (DOM.Node -> VDomMachine a w) -- top hydrate function
  (VDomMachine a w) -- top build function
  i
  (VDomStep a w)

type VDomBuilder4 i j k l a w = EFn.EffectFn6 (VDomSpec a w) (VDomMachine a w) i j k l (VDomStep a w)

type VDomHydrator4 i j k l a w
  = EFn.EffectFn8
  DOM.Node
  (VDomSpecWithHydration a w)
  (DOM.Node -> VDomMachine a w)
  (VDomMachine a w)
  i
  j
  k
  l
  (VDomStep a w)

-- | Widget machines recursively reference the configured spec to potentially
-- | enable recursive trees of Widgets.
newtype VDomSpec a w = VDomSpec
  { buildWidget ∷ VDomSpec a w → Machine w DOM.Node -- `buildWidget` takes a circular reference to the `VDomSpec`
  , buildAttributes ∷ DOM.Element → Machine a Unit
  , document ∷ DOM.Document -- We need document to be able to call `document.createElement` function
  }

newtype VDomSpecWithHydration a w = VDomSpecWithHydration
  { vdomSpec ∷ VDomSpec a w
  , hydrateWidget ∷ VDomSpecWithHydration a w → DOM.Node → Machine w DOM.Node
  , hydrateAttributes ∷ DOM.Element → Machine a Unit
  }
