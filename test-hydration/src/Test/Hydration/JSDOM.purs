module Test.Hydration.JSDOM where

import Effect (Effect)
import Web.HTML.Window as HTML

foreign import data JSDOM ∷ Type

foreign import make ∷ String → Effect JSDOM

foreign import window ∷ JSDOM → Effect HTML.Window

foreign import serialize ∷ JSDOM → Effect String
