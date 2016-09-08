module Halogen.VDom.Util
  ( forE
  , replicateE
  , whenE
  , diffWithIxE
  , diffWithKeyAndIxE
  , strMapWithIxE
  , refEq
  ) where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried as Fn
import Data.StrMap as StrMap

foreign import forE
  ∷ ∀ eff a b
  . Fn.Fn2
      (Array a)
      (Fn.Fn2 Int a (Eff eff b))
      (Eff eff (Array b))

foreign import replicateE
  ∷ ∀ eff a
  . Fn.Fn2 Int (Eff eff a) (Eff eff Unit)

foreign import whenE
  ∷ ∀ eff a
  . Fn.Fn2 Boolean (Eff eff a) (Eff eff Unit)

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
  ∷ ∀ a b
  . Fn.Fn2 a b Boolean
