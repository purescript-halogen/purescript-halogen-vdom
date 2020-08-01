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
import Halogen.VDom.DOM.Prop.Util (unsafeGetProperty)
import Halogen.VDom.Set as Set
import Halogen.VDom.Types (Namespace)
import Halogen.VDom.Util as Util
import Web.DOM.Element (Element) as DOM

checkAttributeExistsAndIsEqual ∷ Maybe Namespace → String → String → DOM.Element → Effect Unit
checkAttributeExistsAndIsEqual maybeNamespace attributeName expectedElementValue element = do
  elementValue ← EFn.runEffectFn3 Util.getAttribute (toNullable maybeNamespace) attributeName element <#> toMaybe

  case elementValue of
    Nothing → do
      EFn.runEffectFn2 Util.warnAny "Error info: " { element }
      throwException $ error $ "Expected element to have an attribute " <> Util.quote (Util.fullAttributeName maybeNamespace attributeName) <> " eq to " <> Util.quote expectedElementValue <> ", but it is missing (check warning above for more information)"
    Just elementValue' →
      unless (elementValue' == expectedElementValue) do
        EFn.runEffectFn2 Util.warnAny "Error info: " { element }
        throwException $ error $ "Expected element to have an attribute " <> Util.quote (Util.fullAttributeName maybeNamespace attributeName) <> " eq to " <> Util.quote expectedElementValue <> ", but it was equal to " <> Util.quote elementValue' <> " (check warning above for more information)"

checkPropExistsAndIsEqual ∷ String → PropValue → DOM.Element → Effect Unit
checkPropExistsAndIsEqual propName expectedPropValue element = do
  let propValue = Fn.runFn2 unsafeGetProperty propName element

  unless (Fn.runFn2 Util.refEq propValue expectedPropValue) do
    EFn.runEffectFn2 Util.warnAny "Error info: " { element, propValue, expectedPropValue }
    throwException $ error $ "Expected element to have a prop " <> Util.quote propName <> " eq to " <> Util.quote (Util.anyToString expectedPropValue) <> ", but it was equal to " <> Util.quote (Util.anyToString propValue) <> " (check warning above for more information)"

-- | Inspired by https://github.com/facebook/react/blob/823dc581fea8814a904579e85a62da6d18258830/packages/react-dom/src/client/ReactDOMComponent.js#L1030
mkExtraAttributeNames ∷ DOM.Element → Effect (Set.Set String)
mkExtraAttributeNames el = do
  let
    namedNodeMap = Attributes.attributes el
  (set ∷ Set.Set String) ← Set.empty
  EFn.runEffectFn2 Attributes.forEachE namedNodeMap (EFn.mkEffectFn1 \attribute → EFn.runEffectFn2 Set.add attribute.name set)
  pure set

checkExtraAttributeNamesIsEmpty ∷ Set.Set String -> DOM.Element -> Effect Unit
checkExtraAttributeNamesIsEmpty extraAttributeNames element =
  when (Set.size extraAttributeNames > 0) do
    EFn.runEffectFn2 Util.warnAny "Error info: " { extraAttributeNames, element }
    throwException $ error $ "Extra attributes from the server: " <> (Set.toArray extraAttributeNames # joinWith ", ") <> " (check warning above for more information)"
