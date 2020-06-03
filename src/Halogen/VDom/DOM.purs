module Halogen.VDom.DOM
  ( module Export
  , buildVDom
  , hydrateVDom
  ) where

import Halogen.VDom.DOM.Elem (buildElem, hydrateElem)
import Halogen.VDom.DOM.Keyed (buildKeyed, hydrateKeyed)
import Halogen.VDom.DOM.Text (buildText, hydrateText)
import Halogen.VDom.DOM.Types (VDomMachine, VDomSpec)
import Halogen.VDom.DOM.Widget (buildWidget, hydrateWidget)

import Effect.Uncurried as EFn
import Halogen.VDom.DOM.Elem (buildElem) as Export
import Halogen.VDom.DOM.Keyed (buildKeyed) as Export
import Halogen.VDom.DOM.Text (buildText) as Export
import Halogen.VDom.DOM.Types (VDomSpec(..)) as Export
import Halogen.VDom.DOM.Widget (buildWidget) as Export
import Halogen.VDom.Types (VDom(..), runGraft)
import Web.DOM.Element (Element) as DOM

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
hydrateVDom spec rootNode = hydrate rootNode
  where
  build = buildVDom spec
  hydrate node = EFn.mkEffectFn1 \vdom -> do
    case vdom of
      Text s → EFn.runEffectFn5 hydrateText node spec hydrate build s
      Elem namespace elemName attribute childrenVdoms → EFn.runEffectFn8 hydrateElem node spec hydrate build namespace elemName attribute childrenVdoms
      Keyed namespace elemName attribute keyedChildrenVdoms → EFn.runEffectFn8 hydrateKeyed node spec hydrate build namespace elemName attribute keyedChildrenVdoms
      Widget w → EFn.runEffectFn5 hydrateWidget node spec hydrate build w
      Grafted g → EFn.runEffectFn1 (hydrate node) (runGraft g)

buildVDom ∷ ∀ a w. VDomSpec a w → VDomMachine a w
buildVDom spec = build
  where
  build = EFn.mkEffectFn1 case _ of
    Text s → EFn.runEffectFn3 buildText spec build s
    Elem namespace elemName a childrenVdoms → EFn.runEffectFn6 buildElem spec build namespace elemName a childrenVdoms
    Keyed namespace elemName a keyedChildrenVdoms → EFn.runEffectFn6 buildKeyed spec build namespace elemName a keyedChildrenVdoms
    Widget w → EFn.runEffectFn3 buildWidget spec build w
    Grafted g → EFn.runEffectFn1 build (runGraft g)
