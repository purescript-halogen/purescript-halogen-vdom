module Halogen.VDom.DOM.Prop
  ( module Export
  , buildProp
  ) where

import Prelude

import Data.Function.Uncurried as Fn
import Data.Maybe (Maybe(..))
import Data.Nullable (null, toNullable, Nullable)
import Data.Tuple (Tuple(..), fst, snd)
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Uncurried as EFn
import Foreign (typeOf)
import Foreign.Object as Object
import Halogen.VDom as V
import Halogen.VDom.Machine (Step, Step'(..), mkStep)
import Halogen.VDom.Types (Namespace(..))
import Halogen.VDom.Util as Util
import Halogen.VDom.Util (STObject')
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM.Element (Element) as DOM
import Web.Event.Event (EventType(..), Event) as DOM
import Web.Event.EventTarget (eventListener, EventListener) as DOM

import Halogen.VDom.DOM.Prop.Implementation
import Halogen.VDom.DOM.Prop.Utils
import Halogen.VDom.DOM.Prop.Types
import Halogen.VDom.DOM.Prop.Types
  ( Prop(..)
  , ElemRef(..)
  , PropValue
  , propFromString
  , propFromBoolean
  , propFromInt
  , propFromNumber
  )
  as Export

-- | A `Machine`` for applying attributes, properties, and event handlers.
-- | An emitter effect must be provided to respond to events. For example,
-- | to allow arbitrary effects in event handlers, one could use `id`.
buildProp
  ∷ ∀ a
  . (a → Effect Unit) -- emitter, for example the global broadcaster function for all elements in halogen component
  → DOM.Element
  → V.Machine (Array (Prop a)) Unit -- Machine takes array of properties for that element, outputs nothing
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
