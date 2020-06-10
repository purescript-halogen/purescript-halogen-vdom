module Halogen.VDom.DOM.Prop.Checkers where

import Prelude

import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe, toNullable)
import Data.String (toLower)
import Data.String.Common (joinWith)
import Effect (Effect)
import Effect.Exception (error, throwException)
import Effect.Uncurried as EFn
import Halogen.VDom.Attributes (attributes, forEachE) as Attributes
import Halogen.VDom.DOM.Prop.Types (PropValue)
import Halogen.VDom.DOM.Prop.Utils (unsafeGetProperty)
import Halogen.VDom.Set as Set
import Halogen.VDom.Types (ElemName(..), Namespace)
import Halogen.VDom.Util (anyToString, fullAttributeName, quote, warnAny)
import Halogen.VDom.Util as Util
import Web.DOM.Element (Element) as DOM

checkAttributeExistsAndIsEqual ∷ Maybe Namespace → String → String → DOM.Element → Effect Unit
checkAttributeExistsAndIsEqual maybeNamespace attributeName expectedElementValue element = do
  elementValue ← (EFn.runEffectFn3 Util.getAttribute (toNullable maybeNamespace) attributeName element) <#> toMaybe
  case elementValue of
    Nothing → do
      EFn.runEffectFn2 warnAny "checkAttributeExistsAndIsEqual -> missing" { element }
      throwException $ error $ "Expected element to have an attribute " <> quote (fullAttributeName maybeNamespace (ElemName attributeName)) <> " eq to " <> quote expectedElementValue <> ", but it is missing"
    Just elementValue' → do
      EFn.runEffectFn2 warnAny "checkAttributeExistsAndIsEqual -> not missing" { elementValue', expectedElementValue, meta: { maybeNamespace, attributeName, expectedElementValue, element } }
      unless (elementValue' == expectedElementValue) (do
        throwException $ error $ "Expected element to have an attribute " <> quote (fullAttributeName maybeNamespace (ElemName attributeName)) <> " eq to " <> quote expectedElementValue <> ", but it was equal to " <> quote elementValue'
        )

checkPropExistsAndIsEqual ∷ String → PropValue → DOM.Element → Effect Unit
checkPropExistsAndIsEqual propName expectedPropValue element = do
  let propValue = Fn.runFn2 unsafeGetProperty propName element
  EFn.runEffectFn2 warnAny "checkPropExistsAndIsEqual" { propValue, expectedPropValue, meta: { element, propName } }
  unless (Fn.runFn2 Util.refEq propValue expectedPropValue) (do
    throwException $ error $ "Expected element to have a prop " <> quote propName <> " eq to " <> quote (anyToString expectedPropValue) <> ", but it was equal to " <> quote (anyToString propValue)
    )

-- | Inspired by https://github.com/facebook/react/blob/823dc581fea8814a904579e85a62da6d18258830/packages/react-dom/src/client/ReactDOMComponent.js#L1030
mkExtraAttributeNames ∷ DOM.Element → Effect (Set.Set String)
mkExtraAttributeNames el = do
  let
    namedNodeMap = Attributes.attributes el
  (set ∷ Set.Set String) ← Set.empty
  EFn.runEffectFn2 Attributes.forEachE namedNodeMap (EFn.mkEffectFn1 \attribute → EFn.runEffectFn2 Set.add (toLower attribute.name) set)
  pure set

checkExtraAttributeNamesIsEmpty ∷ Set.Set String -> DOM.Element -> Effect Unit
checkExtraAttributeNamesIsEmpty extraAttributeNames element = do
  EFn.runEffectFn2 warnAny "checkExtraAttributeNamesIsEmpty" { extraAttributeNames, meta: { element } }
  when (Set.size extraAttributeNames > 0) (do
    throwException $ error $ "Extra attributes from the server: " <> (Set.toArray extraAttributeNames # joinWith ", ")
    )
