module Halogen.VDom.Util where
  -- | ( newMutMap
  -- | , pokeMutMap
  -- | , deleteMutMap
  -- | , unsafeFreeze
  -- | , unsafeLookup
  -- | , unsafeGetAny
  -- | , unsafeHasAny
  -- | , unsafeSetAny
  -- | , unsafeDeleteAny
  -- | , forE
  -- | , forEachE
  -- | , forInE
  -- | , replicateE
  -- | , diffWithIxE
  -- | , diffWithKeyAndIxE
  -- | , strMapWithIxE
  -- | , refEq
  -- | , createTextNode
  -- | , setTextContent
  -- | , createElement
  -- | , insertChildIx
  -- | , removeChild
  -- | , parentNode
  -- | , setAttribute
  -- | , removeAttribute
  -- | , hasAttribute
  -- | , addEventListener
  -- | , removeEventListener
  -- | , JsUndefined
  -- | , jsUndefined
  -- | , STObject'
  -- | , nodeIsTextNode
  -- | , nodeIsElementNode
  -- | ) where

import Prelude

import Data.Function.Uncurried as Fn
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Uncurried as EFn
import Foreign.Object (Object)
import Foreign.Object as Object
import Foreign.Object.ST (STObject)
import Foreign.Object.ST as STObject
import Halogen.VDom.Types (Namespace, ElemName)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Document (Document) as DOM
import Web.DOM.Element (Element) as DOM
import Web.DOM.Node (Node) as DOM
import Web.Event.EventTarget (EventListener) as DOM

data STObject' a -- just like STObject, but without region

newMutMap ∷ ∀ a. Effect (STObject' a)
newMutMap = unsafeCoerce STObject.new

pokeMutMap ∷ ∀ a. EFn.EffectFn3 String a (STObject' a) Unit
pokeMutMap = unsafeSetAny

deleteMutMap ∷ ∀ a. EFn.EffectFn2 String (STObject' a) Unit
deleteMutMap = unsafeDeleteAny

unsafeFreeze ∷ ∀ a. STObject' a → Object a
unsafeFreeze = unsafeCoerce

unsafeLookup ∷ ∀ a. Fn.Fn2 String (Object a) a
unsafeLookup = unsafeGetAny

foreign import unsafeGetAny
  ∷ ∀ a b. Fn.Fn2 String a b

foreign import unsafeHasAny
  ∷ ∀ a. Fn.Fn2 String a Boolean

foreign import unsafeSetAny ∷ ∀ a b. EFn.EffectFn3 String a b Unit

foreign import unsafeDeleteAny
  ∷ ∀ a. EFn.EffectFn2 String a Unit

foreign import forE
  ∷ ∀ a b
  . EFn.EffectFn2
      (Array a)
      (EFn.EffectFn2 Int a b)
      (Array b)

foreign import forEachE
  ∷ ∀ a
  . EFn.EffectFn2
      (Array a)
      (EFn.EffectFn1 a Unit)
      Unit

foreign import forInE
  ∷ ∀ a
  . EFn.EffectFn2
      (Object.Object a)
      (EFn.EffectFn2 String a Unit)
      Unit

foreign import replicateE
  ∷ ∀ a
  . EFn.EffectFn2
      Int
      (Effect a)
      Unit

foreign import diffWithIxE
  ∷ ∀ oldElem newElem output dismissed
  . EFn.EffectFn5
      (Array oldElem) -- e.g. list of vdom elements
      (Array newElem) -- e.g. list of vdom elements
      (EFn.EffectFn3 Int oldElem newElem output) -- execute action when both elems are found in oldElems array and newElems array under the same index (usually used to remove old element from DOM and add new element to DOM)
      (EFn.EffectFn2 Int oldElem dismissed) -- execute action when only oldElem is found, there are no elems left in `Array newElem` (happens when array of old elements is bigger than array of new elements)
      (EFn.EffectFn2 Int newElem output) -- execute action when only newElem is found, there are no elems left in `Array oldElem` (happens when array of new elements is bigger than array of old elements)
      (Array output) -- e.g. list of dom elements

foreign import diffWithKeyAndIxE
  ∷ ∀ oldElem newElemWithKey output dismissed
  . EFn.EffectFn6
      (Object.Object oldElem)
      (Array newElemWithKey)
      (newElemWithKey → String)
      (EFn.EffectFn4 String Int oldElem newElemWithKey output)
      (EFn.EffectFn2 String oldElem dismissed)
      (EFn.EffectFn3 String Int newElemWithKey output)
      (Object.Object output)

foreign import strMapWithIxE
  ∷ ∀ child outputVal
  . EFn.EffectFn3
      (Array child) -- children
      (child → String) -- propToStrKey
      (EFn.EffectFn3 String Int child outputVal) -- action, executed on each array element, (StrKey -> Index -> child -> outputVal)
      (Object.Object outputVal) -- key is StrKey, val type is outputVal

foreign import refEq
  ∷ ∀ a b. Fn.Fn2 a b Boolean

foreign import createTextNode
  ∷ EFn.EffectFn2 String DOM.Document DOM.Node

foreign import setTextContent
  ∷ EFn.EffectFn2 String DOM.Node Unit

foreign import createElement
  ∷ EFn.EffectFn3 (Nullable Namespace) ElemName DOM.Document DOM.Element

-- Insert new child at index
-- (if there is already an element on that index, that old element is moved below).
-- If there are not enough elements - new child is moved at the end of the list.
foreign import insertChildIx
  ∷ EFn.EffectFn3 Int DOM.Node DOM.Node Unit

foreign import removeChild
  ∷ EFn.EffectFn2 DOM.Node DOM.Node Unit

foreign import parentNode
  ∷ EFn.EffectFn1 DOM.Node DOM.Node

foreign import setAttribute
  ∷ EFn.EffectFn4 (Nullable Namespace) String String DOM.Element Unit

foreign import removeAttribute
  ∷ EFn.EffectFn3 (Nullable Namespace) String DOM.Element Unit

foreign import hasAttribute
  ∷ EFn.EffectFn3 (Nullable Namespace) String DOM.Element Boolean

foreign import addEventListener
  ∷ EFn.EffectFn3 String DOM.EventListener DOM.Element Unit

foreign import removeEventListener
  ∷ EFn.EffectFn3 String DOM.EventListener DOM.Element Unit

foreign import data JsUndefined ∷ Type

foreign import jsUndefined ∷ JsUndefined

foreign import getNodeType :: EFn.EffectFn1 DOM.Node Int

foreign import nodeIsTextNode :: EFn.EffectFn1 DOM.Node Boolean

foreign import nodeIsElementNode :: EFn.EffectFn1 DOM.Node Boolean

foreign import getTextContent :: EFn.EffectFn1 DOM.Node String

foreign import getNamespaceURI :: EFn.EffectFn1 DOM.Node (Nullable Namespace)
