module Halogen.VDom.Util
  ( effPure
  , effUnit
  , MutStrMap
  , unsafeNewMutMap
  , unsafePokeMutMap
  , unsafeDeleteMutMap
  , unsafeFreeze
  , unsafeLookup
  , unsafeGetAny
  , unsafeHasAny
  , unsafeSetAny
  , unsafeDeleteAny
  , unsafeForE
  , unsafeForInE
  , unsafeReplicateE
  , unsafeDiffWithIxE
  , unsafeDiffWithKeyAndIxE
  , unsafeStrMapWithIxE
  , refEq
  , unsafeCreateTextNode
  , unsafeSetTextContent
  , unsafeCreateElement
  , unsafeInsertChildIx
  , unsafeRemoveChild
  , unsafeParent
  , unsafeSetAttribute
  , unsafeRemoveAttribute
  , unsafeAddEventListener
  , unsafeRemoveEventListener
  , JsUndefined
  , jsUndefined
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried as Fn
import Data.Nullable (Nullable)
import Data.StrMap (StrMap)
import Data.StrMap as StrMap
import Data.StrMap.ST (STStrMap)
import Data.StrMap.ST as STStrMap
import DOM (DOM)
import DOM.Event.EventTarget (EventListener) as DOM
import DOM.Node.Types (Document, Element, Node) as DOM
import Halogen.VDom.Types (Namespace, ElemName)
import Unsafe.Coerce (unsafeCoerce)

effPure ∷ ∀ eff a. a → Eff eff a
effPure = pure

effUnit ∷ ∀ eff. Eff eff Unit
effUnit = pure unit

type MutStrMap = STStrMap Void

unsafeNewMutMap ∷ ∀ a. MutStrMap a
unsafeNewMutMap = unsafeCoerce STStrMap.new

unsafePokeMutMap ∷ ∀ a. Fn.Fn3 String a (MutStrMap a) Unit
unsafePokeMutMap = unsafeSetAny

unsafeDeleteMutMap ∷ ∀ a. Fn.Fn2 String (MutStrMap a) Unit
unsafeDeleteMutMap = unsafeDeleteAny

unsafeFreeze ∷ ∀ a. MutStrMap a → StrMap a
unsafeFreeze = unsafeCoerce

unsafeLookup ∷ ∀ a. Fn.Fn2 String (StrMap a) a
unsafeLookup = unsafeGetAny

foreign import unsafeGetAny
  ∷ ∀ a b. Fn.Fn2 String a b

foreign import unsafeHasAny
  ∷ ∀ a. Fn.Fn2 String a Boolean

foreign import unsafeSetAny
  ∷ ∀ a b. Fn.Fn3 String a b Unit

foreign import unsafeDeleteAny
  ∷ ∀ a. Fn.Fn2 String a Unit

foreign import unsafeForE
  ∷ ∀ eff a b
  . Fn.Fn2
      (Array a)
      (Fn.Fn2 Int a (Eff eff b))
      (Array b)

foreign import unsafeForInE
  ∷ ∀ eff a
  . Fn.Fn2
      (StrMap.StrMap a)
      (Fn.Fn2 String a (Eff eff Unit))
      Unit

foreign import unsafeReplicateE
  ∷ ∀ eff a
  . Fn.Fn2
      Int
      (Eff eff a)
      Unit

foreign import unsafeDiffWithIxE
  ∷ ∀ eff b c d
  . Fn.Fn5
      (Array b)
      (Array c)
      (Fn.Fn3 Int b c (Eff eff d))
      (Fn.Fn2 Int b (Eff eff Unit))
      (Fn.Fn2 Int c (Eff eff d))
      (Array d)

foreign import unsafeDiffWithKeyAndIxE
  ∷ ∀ eff a b c d
  . Fn.Fn6
      (StrMap.StrMap a)
      (Array b)
      (b → String)
      (Fn.Fn4 String Int a b (Eff eff c))
      (Fn.Fn2 String a (Eff eff d))
      (Fn.Fn3 String Int b (Eff eff c))
      (StrMap.StrMap c)

foreign import unsafeStrMapWithIxE
  ∷ ∀ eff a b
  . Fn.Fn3
      (Array a)
      (a → String)
      (Fn.Fn3 String Int a (Eff eff b))
      (StrMap.StrMap b)

foreign import refEq
  ∷ ∀ a b. Fn.Fn2 a b Boolean

foreign import unsafeCreateTextNode
  ∷ Fn.Fn2 String DOM.Document DOM.Node

foreign import unsafeSetTextContent
  ∷ Fn.Fn2 String DOM.Node Unit

foreign import unsafeCreateElement
  ∷ Fn.Fn3 (Nullable Namespace) ElemName DOM.Document DOM.Element

foreign import unsafeInsertChildIx
  ∷ Fn.Fn3 Int DOM.Node DOM.Node Unit

foreign import unsafeRemoveChild
  ∷ Fn.Fn2 DOM.Node DOM.Node Unit

foreign import unsafeParent
  ∷ DOM.Node → DOM.Node

foreign import unsafeSetAttribute
  ∷ Fn.Fn4 (Nullable Namespace) String String DOM.Element Unit

foreign import unsafeRemoveAttribute
  ∷ Fn.Fn3 (Nullable Namespace) String DOM.Element Unit

foreign import unsafeAddEventListener
  ∷ ∀ eff. Fn.Fn3 String (DOM.EventListener (dom ∷ DOM | eff)) DOM.Element Unit

foreign import unsafeRemoveEventListener
  ∷ ∀ eff. Fn.Fn3 String (DOM.EventListener (dom ∷ DOM | eff)) DOM.Element Unit

foreign import data JsUndefined ∷ Type

foreign import jsUndefined ∷ JsUndefined
