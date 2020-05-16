module Halogen.VDom.DOM.Types where

import Prelude

import Data.Array as Array
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Tuple (Tuple(..), fst)
import Effect (Effect)
import Effect.Uncurried as EFn
import Foreign.Object as Object
import Halogen.VDom.Machine (Machine, Step, Step'(..), extract, halt, mkStep, step, unStep)
import Halogen.VDom.Machine as Machine
import Halogen.VDom.Types (ElemName(..), Namespace(..), VDom(..), runGraft)
import Halogen.VDom.Util as Util
import Halogen.VDom.DOM.Prop.Types (BuildPropFunction, Prop)
import Web.DOM.Document (Document) as DOM
import Web.DOM.Element (Element) as DOM
import Web.DOM.Element as DOM.Element
import Web.DOM.Node (Node) as DOM

-- A function, that takes `VDom a w` and builds a `DOM.Node`
type VDomMachine a w = Machine (VDom a w) DOM.Node

type VDomStep a w = Step (VDom a w) DOM.Node

type VDomInit i a w = EFn.EffectFn1 i (VDomStep a w)

-- Equal to
-- (VDomSpec a w) -> (VDOM a w -> Step (VDOM a w) DOM.Node) -> i -> Effect (Step (VDOM a w) DOM.Node)
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
  -- example:

  -- buildAttributes = buildProps handler
  -- https://github.com/purescript-halogen/purescript-halogen/blob/bb715fe5c06ba3048f4d8b377ec842cd8cf37833/src/Halogen/VDom/Driver.purs#L68-L71

  -- what is handler
  -- https://github.com/purescript-halogen/purescript-halogen/blob/bb715fe5c06ba3048f4d8b377ec842cd8cf37833/src/Halogen/Aff/Driver.purs#L203
  , buildAttributes ∷ DOM.Element → Machine a Unit
  , hydrateAttributes ∷ DOM.Element → Machine a Unit

  -- We need document to be able to call `document.createElement` function
  , document ∷ DOM.Document
  }
