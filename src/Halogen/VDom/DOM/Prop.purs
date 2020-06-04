module Halogen.VDom.DOM.Prop
  ( module Export
  , buildProp
  , hydrateProp
  ) where

import Prelude
import Halogen.VDom.DOM.Prop.Implementation (applyProp, diffProp, hydrateApplyProp, mbEmit, removeProp)
import Halogen.VDom.DOM.Prop.Types (ElemRef(..), EventListenerAndCurrentEmitterInputBuilder, Prop(..), PropState, BuildPropFunction)
import Halogen.VDom.DOM.Prop.Utils (propToStrKey)
import Halogen.VDom.Util (STObject')

import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Effect.Uncurried as EFn
import Foreign.Object as Object
import Halogen.VDom.DOM.Prop.Types (Prop(..), ElemRef(..), PropValue, propFromString, propFromBoolean, propFromInt, propFromNumber) as Export
import Halogen.VDom.Machine (Step, Step'(..), mkStep)
import Halogen.VDom.Util as Util
import Halogen.VDom.DOM.Prop.Checkers (mkExtraAttributeNames, checkExtraAttributeNamesIsEmpty)

hydrateProp
  ∷ ∀ a
  . BuildPropFunction a
hydrateProp emit el = renderProp
  where
  renderProp ∷ EFn.EffectFn1 (Array (Prop a)) (Step (Array (Prop a)) Unit)
  renderProp = EFn.mkEffectFn1 \ps1 → do
    (events ∷ STObject' (EventListenerAndCurrentEmitterInputBuilder a)) ← Util.newMutMap

    extraAttributeNames ← mkExtraAttributeNames el

    (props ∷ Object.Object (Prop a)) ← EFn.runEffectFn3 Util.strMapWithIxE ps1 propToStrKey (Fn.runFn4 hydrateApplyProp extraAttributeNames el emit events)

    checkExtraAttributeNamesIsEmpty extraAttributeNames el

    let
      (state ∷ PropState a) =
        { events: Util.unsafeFreeze events
        , props
        , el
        , emit
        }
    pure $ mkStep $ Step unit state patchProp haltProp

-- | A `Machine`` for applying attributes, properties, and event handlers.
-- | An emitter effect must be provided to respond to events. For example,
-- | to allow arbitrary effects in event handlers, one could use `id`.
buildProp
  ∷ ∀ a
  . BuildPropFunction a
buildProp emit el = renderProp
  where
  renderProp ∷ EFn.EffectFn1 (Array (Prop a)) (Step (Array (Prop a)) Unit)
  renderProp = EFn.mkEffectFn1 \ps1 → do
    (events ∷ STObject' (EventListenerAndCurrentEmitterInputBuilder a)) ← Util.newMutMap
    (props ∷ Object.Object (Prop a)) ← EFn.runEffectFn3 Util.strMapWithIxE ps1 propToStrKey (Fn.runFn3 applyProp el emit events)
    let
      (state ∷ PropState a) =
        { events: Util.unsafeFreeze events
        , props
        , el
        , emit
        }
    pure $ mkStep $ Step unit state patchProp haltProp

patchProp ::
  ∀ a
  . EFn.EffectFn2
  (PropState a)
  (Array (Prop a))
  (Step (Array (Prop a)) Unit)
patchProp = EFn.mkEffectFn2 \state ps2 → do
  events ← Util.newMutMap
  let
    { events: prevEvents, props: ps1, emit, el } = state
    onThese = Fn.runFn3 diffProp el prevEvents events
    onThis = Fn.runFn2 removeProp el prevEvents
    onThat = Fn.runFn3 applyProp el emit events
  props ← EFn.runEffectFn6 Util.diffWithKeyAndIxE ps1 ps2 propToStrKey onThese onThis onThat
  let
    nextState = -- TODO: reuse prev object
      { events: Util.unsafeFreeze events
      , props
      , el
      , emit
      }
  pure $ mkStep $ Step unit nextState patchProp haltProp

haltProp
  ∷ ∀ a
  . EFn.EffectFn1
  (PropState a)
  Unit
haltProp = EFn.mkEffectFn1 \state → do
  case Object.lookup "ref" state.props of
    Just (Ref emitterInputBuilder) →
      EFn.runEffectFn2 mbEmit state.emit (emitterInputBuilder (Removed state.el))
    _ → pure unit
