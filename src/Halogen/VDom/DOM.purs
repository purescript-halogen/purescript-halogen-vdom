module Halogen.VDom.DOM
  ( module Export
  , buildVDom
  , hydrateVDom
  ) where

import Prelude

import Effect.Uncurried as EFn
import Halogen.VDom.DOM.Elem (buildElem) as Export
import Halogen.VDom.DOM.Elem (buildElem, hydrateElem)
import Halogen.VDom.DOM.Keyed (buildKeyed) as Export
import Halogen.VDom.DOM.Keyed (buildKeyed, hydrateKeyed)
import Halogen.VDom.DOM.Text (buildText) as Export
import Halogen.VDom.DOM.Text (buildText, hydrateText)
import Halogen.VDom.DOM.Types (VDomMachine, VDomSpec, VDomSpecWithHydration(..))
import Halogen.VDom.DOM.Types (VDomSpec(..), VDomSpecWithHydration(..)) as Export
import Halogen.VDom.DOM.Widget (buildWidget) as Export
import Halogen.VDom.DOM.Widget (buildWidget, hydrateWidget)
import Halogen.VDom.Types (VDom(..), runGraft)
import Halogen.VDom.Util (warnAny)
import Web.DOM.Node (Node) as DOM

hydrateVDom ∷ ∀ a w. VDomSpecWithHydration a w → DOM.Node -> VDomMachine a w
hydrateVDom specWithHydration@(VDomSpecWithHydration specWithHydration') rootNode = hydrate rootNode
  where
  build = buildVDom specWithHydration'.vdomSpec
  hydrate node = EFn.mkEffectFn1 \vdom -> do
    EFn.runEffectFn2 warnAny "hydrate" { node, vdom }
    case vdom of
      Text s → EFn.runEffectFn5 hydrateText node specWithHydration hydrate build s
      Elem namespace elemName attribute childrenVdoms → EFn.runEffectFn8 hydrateElem node specWithHydration hydrate build namespace elemName attribute childrenVdoms
      Keyed namespace elemName attribute keyedChildrenVdoms → EFn.runEffectFn8 hydrateKeyed node specWithHydration hydrate build namespace elemName attribute keyedChildrenVdoms
      Widget w → EFn.runEffectFn5 hydrateWidget node specWithHydration hydrate build w
      Grafted g → EFn.runEffectFn1 (hydrate node) (runGraft g)

-- | Starts an initial `VDom` machine by providing a `VDomSpec`.
-- |
-- | ```purescript
-- | main = do
-- |   machine1 ← buildVDom spec vdomTree1
-- |   machine2 ← Machine.step machine1 vdomTree2
-- |   machine3 ← Machine.step machine2 vdomTree3
-- |   ...
-- | ````
buildVDom ∷ ∀ a w. VDomSpec a w → VDomMachine a w
buildVDom spec = build
  where
  build = EFn.mkEffectFn1 \vdom -> do
    EFn.runEffectFn2 warnAny "build" { vdom }
    case vdom of
      Text s → EFn.runEffectFn3 buildText spec build s
      Elem namespace elemName a childrenVdoms → EFn.runEffectFn6 buildElem spec build namespace elemName a childrenVdoms
      Keyed namespace elemName a keyedChildrenVdoms → EFn.runEffectFn6 buildKeyed spec build namespace elemName a keyedChildrenVdoms
      Widget w → EFn.runEffectFn3 buildWidget spec build w
      Grafted g → EFn.runEffectFn1 build (runGraft g)
