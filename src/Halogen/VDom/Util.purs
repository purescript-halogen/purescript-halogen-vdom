module Halogen.VDom.Util
  ( effPure
  , effUnit
  , MutStrMap
  , newMutMap
  , pokeMutMap
  , deleteMutMap
  , unsafeFreeze
  , unsafeLookup
  , unsafeGetAny
  , unsafeHasAny
  , unsafeSetAny
  , unsafeDeleteAny
  , forE
  , forInE
  , replicateE
  , diffWithIxE
  , diffWithKeyAndIxE
  , strMapWithIxE
  , refEq
  , createTextNode
  , setTextContent
  , createElement
  , insertChildIx
  , removeChild
  , unsafeParent
  , setAttribute
  , removeAttribute
  , addEventListener
  , removeEventListener
  , JsUndefined
  , jsUndefined
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Ref (REF)
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

newMutMap ∷ ∀ eff a. Eff (ref ∷ REF | eff) (MutStrMap a)
newMutMap = unsafeCoerce STStrMap.new

pokeMutMap ∷ ∀ eff a. Fn.Fn3 String a (MutStrMap a) (Eff (ref ∷ REF | eff) Unit)
pokeMutMap = unsafeSetAny

deleteMutMap ∷ ∀ eff a. Fn.Fn2 String (MutStrMap a) (Eff (ref ∷ REF | eff) Unit)
deleteMutMap = unsafeDeleteAny

unsafeFreeze ∷ ∀ a. MutStrMap a → StrMap a
unsafeFreeze = unsafeCoerce

unsafeLookup ∷ ∀ a. Fn.Fn2 String (StrMap a) a
unsafeLookup = unsafeGetAny

foreign import unsafeGetAny
  ∷ ∀ a b. Fn.Fn2 String a b

foreign import unsafeHasAny
  ∷ ∀ a. Fn.Fn2 String a Boolean

foreign import unsafeSetAny
  ∷ ∀ eff a b. Fn.Fn3 String a b (Eff eff Unit)

foreign import unsafeDeleteAny
  ∷ ∀ eff a. Fn.Fn2 String a (Eff eff Unit)

foreign import forE
  ∷ ∀ eff a b
  . Fn.Fn2
      (Array a)
      (Fn.Fn2 Int a (Eff eff b))
      (Eff eff (Array b))

foreign import forInE
  ∷ ∀ eff a
  . Fn.Fn2
      (StrMap.StrMap a)
      (Fn.Fn2 String a (Eff eff Unit))
      (Eff eff Unit)

foreign import replicateE
  ∷ ∀ eff a
  . Fn.Fn2
      Int
      (Eff eff a)
      (Eff eff Unit)

foreign import diffWithIxE
  ∷ ∀ eff b c d
  . Fn.Fn5
      (Array b)
      (Array c)
      (Fn.Fn3 Int b c (Eff eff d))
      (Fn.Fn2 Int b (Eff eff Unit))
      (Fn.Fn2 Int c (Eff eff d))
      (Eff eff (Array d))

foreign import diffWithKeyAndIxE
  ∷ ∀ eff a b c d
  . Fn.Fn6
      (StrMap.StrMap a)
      (Array b)
      (b → String)
      (Fn.Fn4 String Int a b (Eff eff c))
      (Fn.Fn2 String a (Eff eff d))
      (Fn.Fn3 String Int b (Eff eff c))
      (Eff eff (StrMap.StrMap c))

foreign import strMapWithIxE
  ∷ ∀ eff a b
  . Fn.Fn3
      (Array a)
      (a → String)
      (Fn.Fn3 String Int a (Eff eff b))
      (Eff eff (StrMap.StrMap b))

foreign import refEq
  ∷ ∀ a b. Fn.Fn2 a b Boolean

foreign import createTextNode
  ∷ ∀ eff
  . Fn.Fn2 String DOM.Document (Eff (dom ∷ DOM | eff) DOM.Node)

foreign import setTextContent
  ∷ ∀ eff
  . Fn.Fn2 String DOM.Node (Eff (dom ∷ DOM | eff) Unit)

foreign import createElement
  ∷ ∀ eff
  . Fn.Fn3 (Nullable Namespace) ElemName DOM.Document (Eff (dom ∷ DOM | eff) DOM.Element)

foreign import insertChildIx
  ∷ ∀ eff
  . Fn.Fn4 String Int DOM.Node DOM.Node (Eff (dom ∷ DOM | eff) Unit)

foreign import removeChild
  ∷ ∀ eff
  . Fn.Fn2 DOM.Node DOM.Node (Eff (dom ∷ DOM | eff) Unit)

foreign import unsafeParent
  ∷ DOM.Node → DOM.Node

foreign import setAttribute
  ∷ ∀ eff. Fn.Fn4 (Nullable Namespace) String String DOM.Element (Eff (dom ∷ DOM | eff) Unit)

foreign import removeAttribute
  ∷ ∀ eff. Fn.Fn3 (Nullable Namespace) String DOM.Element (Eff (dom ∷ DOM | eff) Unit)

foreign import addEventListener
  ∷ ∀ eff. Fn.Fn3 String (DOM.EventListener (dom ∷ DOM | eff)) DOM.Element (Eff (dom ∷ DOM | eff) Unit)

foreign import removeEventListener
  ∷ ∀ eff. Fn.Fn3 String (DOM.EventListener (dom ∷ DOM | eff)) DOM.Element (Eff (dom ∷ DOM | eff) Unit)

foreign import data JsUndefined ∷ Type

foreign import jsUndefined ∷ JsUndefined
