module Halogen.VDom.DOM.Prop.Types where

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

-- | Attributes, properties, event handlers, and element lifecycles.
-- | Parameterized by the type of handlers outputs.

-- | What is the difference between attributes and properties?
-- |
-- | Attributes are defined by HTML. Properties (on DOM elements) are defined by DOM.
-- | E.g. `class` attribute corresponds to `element.className` property
-- | almost always you should use properties on html elements, the svg elements don't have properties, only classes
-- | more https://github.com/purescript-halogen/purescript-halogen-vdom/issues/30#issuecomment-518015764
-- |
-- | Also, attributes can be only strings, props - strings, numbers, booleans
data Prop a
  = Attribute
    -- XML namespace
    (Maybe Namespace)
    -- Attribute name
    String
    -- Attribute value
    String
  | Property
    -- Property name. Usually is equal to attribute name, exeptions are: "htmlFor" property is a "for" attribute, "className" - "class"
    String
    PropValue
  | Handler
    -- Event type to listen to
    DOM.EventType
    -- Function that builds input for emitter (EmitterInputBuilder), if Nothing is returned - emitter is not called
    -- NOTE: If multiple event handlers are added for the same event for the same element - only last event handler is going to work
    -- (e.g. like in `H.div [HP.eventHandler (...), HP.eventHandler (...)]`)
    (DOM.Event → Maybe a)
  | Ref
    -- This function builds input for emitter function too, but when parent element is removed or created
    -- If Nothing is returned - emitter is not called
    -- NOTE: If multiple ref handlers are added for the same element - only last ref handler is going to work
    (ElemRef DOM.Element → Maybe a)

instance functorProp ∷ Functor Prop where
  map f (Handler ty g) = Handler ty (map f <$> g)
  map f (Ref g) = Ref (map f <$> g)
  map f p = unsafeCoerce p

data ElemRef a
  = Created a
  | Removed a

instance functorElemRef ∷ Functor ElemRef where
  map f (Created a) = Created (f a)
  map f (Removed a) = Removed (f a)

foreign import data PropValue ∷ Type

propFromString ∷ String → PropValue
propFromString = unsafeCoerce

propFromBoolean ∷ Boolean → PropValue
propFromBoolean = unsafeCoerce

propFromInt ∷ Int → PropValue
propFromInt = unsafeCoerce

propFromNumber ∷ Number → PropValue
propFromNumber = unsafeCoerce

type EmitterInputBuilder a = DOM.Event → Maybe a
type EventListenerAndCurrentEmitterInputBuilder a = Tuple DOM.EventListener (Ref.Ref (EmitterInputBuilder a))

type PropState a =
  { events ∷ Object.Object (EventListenerAndCurrentEmitterInputBuilder a)
  , props ∷ Object.Object (Prop a)
  , el ∷ DOM.Element
  , emit ∷ a → Effect Unit
  }
