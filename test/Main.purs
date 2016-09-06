module Test.Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Timer as Timer
import Data.Nullable (toMaybe)
import Data.Foldable (for_)
import Data.StrMap as StrMap
import Data.Tuple (Tuple(..))
import DOM (DOM)
import DOM.HTML (window)
import DOM.HTML.Types (htmlDocumentToDocument, htmlDocumentToParentNode)
import DOM.HTML.Window (document)
import DOM.Node.Element (setAttribute, removeAttribute)
import DOM.Node.Types (Document, Element, elementToNode)
import DOM.Node.Node (appendChild)
import DOM.Node.ParentNode (querySelector)
import Halogen.VDom as V
import Halogen.VDom.Machine as M

infixr 1 Tuple as :=

type Attribute = Tuple String String

type BareDom = V.VDom Void (Array Attribute) Void

buildAttributes ∷ ∀ eff. Element → V.VDomMachine eff (Array Attribute) Unit
buildAttributes elem = apply
  where
  apply as1 = do
    let
      as1' = sort as1
    StrMap.foldM (\_ k v → setAttribute k v elem) unit as1'
    pure (M.Step unit (patch as1') done)

  patch as1 as2 = do
    let
      as2' = sort as2
    StrMap.foldM
      (\_ k v →
        if StrMap.member k as2'
          then setAttribute k v elem
          else removeAttribute k elem)
      unit
      (StrMap.union as2' as1)
    pure (M.Step unit (patch as2') done)

  sort = StrMap.fromFoldable
  done = pure unit

dom1 ∷ BareDom
dom1 =
  V.elem "div"
    [ "class" := "root" ]
    [ V.elem "h1"
        [ "style" := "text-transform: uppercase" ]
        [ V.text "Hello, world!" ]
    , V.elem "p" []
        [ V.text "This is a rendered document" ]
    ]

dom2 ∷ BareDom
dom2 =
  V.elem "div"
    [ "class" := "root" ]
    [ V.elem "h1"
        [ "style" := "text-transform: uppercase" ]
        [ V.text "Hello, universe!" ]
    , V.elem "div" []
        [ V.text "Yo yo yo" ]
    ]

renderSpec ∷ ∀ eff. Document → V.VDomSpec eff (Array Attribute) Void
renderSpec =
  { buildWidget: M.stepper (pure <<< absurd) (pure unit)
  , buildAttributes
  , document: _
  }

main :: ∀ eff. Eff (dom ∷ DOM, timer ∷ Timer.TIMER | eff) Unit
main = do
  win ← window
  doc ← document win
  bod ← querySelector "body" (htmlDocumentToParentNode doc)
  for_ (toMaybe bod) \body → do
    let
      spec = renderSpec (htmlDocumentToDocument doc)
    machine ← V.buildVDom spec dom1
    appendChild (M.extract machine) (elementToNode body)
    Timer.setTimeout 3000 $ void $
      M.step machine dom2
