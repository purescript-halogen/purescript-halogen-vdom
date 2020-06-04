module Halogen.VDom.DOM.Prop.Checkers where

import Halogen.VDom.DOM.Prop.Types (PropValue)
import Halogen.VDom.DOM.Prop.Utils (unsafeGetProperty)
import Prelude (Unit, bind, pure, unit, ($), (<#>), (<>), (==), discard)

import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Nullable (toMaybe, toNullable)
import Effect (Effect)
import Effect.Uncurried as EFn
import Halogen.VDom.Types (ElemName(..), Namespace)
import Halogen.VDom.Util (anyToString, fullAttributeName, quote, warnAny)
import Halogen.VDom.Util as Util
import Web.DOM.Element (Element) as DOM
import Effect.Exception (error, throwException)

checkAttributeExistsAndIsEqual ∷ Maybe Namespace → String → String → DOM.Element → Effect Unit
checkAttributeExistsAndIsEqual maybeNamespace attributeName expectedElementValue element = do
  elementValue ← (EFn.runEffectFn3 Util.getAttribute (toNullable maybeNamespace) attributeName element) <#> toMaybe
  case elementValue of
    Nothing → do
      EFn.runEffectFn2 warnAny "Error at " { element }
      throwException $ error $ "Expected element to have an attribute " <> quote (fullAttributeName maybeNamespace (ElemName attributeName)) <> " eq to " <> quote expectedElementValue <> ", but it is missing"
    Just elementValue' →
      if elementValue' == expectedElementValue
        then pure unit
        else do
          EFn.runEffectFn2 warnAny "Error at " { element }
          throwException $ error $ "Expected element to have an attribute " <> quote (fullAttributeName maybeNamespace (ElemName attributeName)) <> " eq to " <> quote expectedElementValue <> ", but it was equal to " <> quote elementValue'

checkPropExistsAndIsEqual ∷ String → PropValue → DOM.Element → Effect Unit
checkPropExistsAndIsEqual propName expectedPropValue element = do
  let propValue = Fn.runFn2 unsafeGetProperty propName element
  if Fn.runFn2 Util.refEq propValue expectedPropValue
    then pure unit
    else do
      EFn.runEffectFn2 warnAny "Error at " { element }
      throwException $ error $ "Expected element to have a prop " <> quote propName <> " eq to " <> quote (anyToString expectedPropValue) <> ", but it was equal to " <> quote (anyToString propValue)
