module Halogen.VDom.DOM.Prop
  ( module Export
  , buildProp
  , hydrateProp
  ) where

import Data.String.Common (joinWith)
import Halogen.VDom.DOM.Prop.Implementation (applyProp, diffProp, hydrateApplyProp, mbEmit, removeProp)
import Halogen.VDom.DOM.Prop.Types (ElemRef(..), EventListenerAndCurrentEmitterInputBuilder, Prop(..), PropState)
import Halogen.VDom.DOM.Prop.Utils (propToStrKey)
import Halogen.VDom.Util (STObject')
import Prelude (Unit, bind, discard, pure, unit, when, (#), ($), (<>), (>))

import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Exception (error, throwException)
import Effect.Uncurried as EFn
import Foreign.Object as Object
import Halogen.VDom.Attributes (attributes, forEachE) as Attributes
import Halogen.VDom.DOM.Prop.Types (Prop(..), ElemRef(..), PropValue, propFromString, propFromBoolean, propFromInt, propFromNumber) as Export
import Halogen.VDom.Machine (Step, Step'(..), mkStep, Machine)
import Halogen.VDom.Set as Set
import Halogen.VDom.Util as Util
import Web.DOM.Element (Element) as DOM
import Halogen.VDom.DOM.Prop.Types (BuildPropFunction)

-- inspired by https://github.com/facebook/react/blob/823dc581fea8814a904579e85a62da6d18258830/packages/react-dom/src/client/ReactDOMComponent.js#L1030
mkExtraAttributeNames ∷ DOM.Element → Effect (Set.Set String)
mkExtraAttributeNames el = do
  let
    namedNodeMap = Attributes.attributes el

  (set ∷ Set.Set String) ← Set.empty
  EFn.runEffectFn2 Attributes.forEachE namedNodeMap (EFn.mkEffectFn1 \name → EFn.runEffectFn2 Set.add name set)
  pure set

throwErrorIfExtraAttributeNamesNonEmpty ∷ Set.Set String → Effect Unit
throwErrorIfExtraAttributeNamesNonEmpty extraAttributeNames = do
  when (Set.size extraAttributeNames > 0)
    (do
    throwException $ error $ "Extra attributes from the server: " <> (Set.toArray extraAttributeNames # joinWith ", ")
    )

hydrateProp
  ∷ ∀ a
  . BuildPropFunction a
hydrateProp emit el = renderProp
  where
  renderProp ∷ EFn.EffectFn1 (Array (Prop a)) (Step (Array (Prop a)) Unit)
  renderProp = EFn.mkEffectFn1 \ps1 → do
    (events ∷ STObject' (EventListenerAndCurrentEmitterInputBuilder a)) ← Util.newMutMap

    extraAttributeNames ← mkExtraAttributeNames el

    -- for each prop in array:
    --   if prop is attr - dont set attr to element, store attr under "attr/XXX" key in a returned object
    --   if prop is property - dont set property to element, store property under "prop/XXX" key in a returned object
    --   if prop is handler for DOM.EventType - start listen and add listener to `events` mutable map, store handler under "handler/EVENTTYPE" in a returned object
    --   if prop is ref updater - store `emitterInputBuilder` in under a `ref` key in a returned object, call `emitter` on creation of all props (now) and on halt of all props (later)
    (props ∷ Object.Object (Prop a)) ← EFn.runEffectFn3 Util.strMapWithIxE ps1 propToStrKey (Fn.runFn4 hydrateApplyProp extraAttributeNames el emit events)
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
  -- what it does - creates a machine, that contains state
  -- on next step - patches prop
  -- on halt - all ref watchers are notified that element is removed

  renderProp ∷ EFn.EffectFn1 (Array (Prop a)) (Step (Array (Prop a)) Unit)
  renderProp = EFn.mkEffectFn1 \ps1 → do
    (events ∷ STObject' (EventListenerAndCurrentEmitterInputBuilder a)) ← Util.newMutMap

    -- for each prop in array:
    --   if prop is attr - set attr to element, store attr under "attr/XXX" key in a returned object
    --   if prop is property - set property to element, store property under "prop/XXX" key in a returned object
    --   if prop is handler for DOM.EventType - start listen and add listener to `events` mutable map, store handler under "handler/EVENTTYPE" in a returned object
    --   if prop is ref updater - store `emitterInputBuilder` in under a `ref` key in a returned object, call `emitter` on creation of all props (now) and on halt of all props (later)
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
