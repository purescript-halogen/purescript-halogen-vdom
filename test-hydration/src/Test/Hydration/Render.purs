module Test.Hydration.Render where

import Halogen.VDom.DOM.StringRenderer as DSR
import Halogen.VDom.Thunk as VT
import Test.Hydration.Basic as Basic

renderWidget ∷ ∀ a. Basic.Widget a → String
renderWidget thunk = render (VT.runThunk thunk)

renderCore ∷ ∀ a. Basic.Core a → String
renderCore core = DSR.render renderWidget core

render ∷ ∀ a. Basic.VDom a → String
render (Basic.VDom core) = renderCore core
