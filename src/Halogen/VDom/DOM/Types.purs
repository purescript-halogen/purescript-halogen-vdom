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
  DOM.Element -- current element
  (VDomSpec a w)
  (DOM.Element -> VDomMachine a w) -- top hydrate function
  (VDomMachine a w) -- top build function
  i
  (VDomStep a w)

type VDomBuilder4 i j k l a w = EFn.EffectFn6 (VDomSpec a w) (VDomMachine a w) i j k l (VDomStep a w)

type VDomHydrator4 i j k l a w
  = EFn.EffectFn8
  DOM.Element
  (VDomSpec a w)
  (DOM.Element -> VDomMachine a w)
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
  , hydrateWidget ∷ VDomSpec a w → DOM.Element → Machine w DOM.Node

  , buildAttributes ∷ DOM.Element → Machine a Unit
  , hydrateAttributes ∷ DOM.Element → Machine a Unit

  -- We need document to be able to call `document.createElement` function
  , document ∷ DOM.Document
  }
