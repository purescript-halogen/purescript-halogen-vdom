module Halogen.VDom.DOM
  ( module Export
  , buildVDom
  , hydrateVDom
  ) where

import Halogen.VDom.DOM.Elem
import Halogen.VDom.DOM.Keyed
import Halogen.VDom.DOM.Text
import Halogen.VDom.DOM.Types
import Halogen.VDom.DOM.Widget
import Prelude

import Data.Array as Array
import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Nullable (toNullable)
import Data.Tuple (Tuple(..), fst)
import Effect.Uncurried as EFn
import Foreign.Object as Object
import Halogen.VDom.DOM.Elem (buildElem) as Export
import Halogen.VDom.DOM.Keyed (buildKeyed) as Export
import Halogen.VDom.DOM.Text (buildText) as Export
import Halogen.VDom.DOM.Types (VDomSpec(..)) as Export
import Halogen.VDom.DOM.Utils (undefined)
import Halogen.VDom.DOM.Widget (buildWidget) as Export
import Halogen.VDom.Machine (Machine, Step, Step'(..), extract, halt, mkStep, step, unStep)
import Halogen.VDom.Machine as Machine
import Halogen.VDom.Types (ElemName(..), Namespace(..), VDom(..), runGraft)
import Halogen.VDom.Util as Util
import Web.DOM.Document (Document) as DOM
import Web.DOM.Element (Element) as DOM
import Web.DOM.Element as DOMElement
import Web.DOM.Node (Node) as DOM

-- | Starts an initial `VDom` machine by providing a `VDomSpec`.
-- |
-- | ```purescript
-- | main = do
-- |   machine1 ← buildVDom spec vdomTree1
-- |   machine2 ← Machine.step machine1 vdomTree2
-- |   machine3 ← Machine.step machine2 vdomTree3
-- |   ...
-- | ````
hydrateVDom ∷ ∀ a w. VDomSpec a w → DOM.Element -> VDomMachine a w
hydrateVDom spec rootNode = hydrate
  where
  build = buildVDom spec
  hydrate = EFn.mkEffectFn1 case _ of
    Text s → EFn.runEffectFn5 hydrateText rootNode spec hydrate build s
    Elem namespace elemName attribute childrenVdoms → EFn.runEffectFn8 hydrateElem rootNode spec hydrate build namespace elemName attribute childrenVdoms
    Keyed namespace elemName attribute keyedChildrenVdoms → undefined
    Widget w → undefined
    Grafted g → undefined

buildVDom ∷ ∀ a w. VDomSpec a w → VDomMachine a w
buildVDom spec = build
  where
  build = EFn.mkEffectFn1 case _ of
    Text s → EFn.runEffectFn3 buildText spec build s -- build text machine
    Elem namespace elemName a childrenVdoms → EFn.runEffectFn6 buildElem spec build namespace elemName a childrenVdoms
    Keyed namespace elemName a keyedChildrenVdoms → EFn.runEffectFn6 buildKeyed spec build namespace elemName a keyedChildrenVdoms
    Widget w → EFn.runEffectFn3 buildWidget spec build w -- machine that has full control of it's lifecycle
    Grafted g → EFn.runEffectFn1 build (runGraft g) -- optimization
