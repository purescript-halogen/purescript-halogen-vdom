# Usage Guide

## Overview

`Halogen.VDom` is built on [Mealy machines](https://en.wikipedia.org/wiki/Mealy_machine).
Given an input `VDom`, the machine yields a `Node`, the next machine step, and a finalizer.

```purescript
import Halogen.VDom as VDom

type MyVDom = VDom.VDom ...

render ∷ MyState → MyVDom

main = do
  -- Build the initial machine
  machine1 ← V.buildVDom myVDomSpec (render state1)

  -- Attach the output node to the DOM
  appendChildToBody (V.extract machine1)

  -- Patch
  machine2 ← V.step machine1 (render state2)
  machine3 ← V.step machine2 (render state3)
  ...
```

Out of the box, only very basic text and node creation is supported. Attributes,
properties, event listeners, hooks, etc. are left for library implementors to
plug in as needed. Library authors should likely `newtype` their wrappers to
get more convenient instances (eg. `map`ping over inputs from event listeners).

## Extending

The core `VDom a w` type is parameterized by the types for element attributes
and custom widgets. Element attributes will likely be a sum for the usual suspects
(DOM attributes, properties, event listeners, lifecycle hooks) and mutate a
given DOM `Element`, while widgets give you complete control over the patching
and diffing of a tree (eg. thunks, custom components, etc).

When you start your initial machine, you provide a `VDomSpec`, which contains
the machines for running your attributes and widgets.

```purescript
import Halogen.VDom as VDom

data MyAttribute
data MyWidget

makeSpec ∷ ∀ eff. DOM.Document → VDom.VDomSpec eff MyAttribute MyWidget
makeSpec document =
  VDom.VDomSpec
    { buildWidget: ...
    , buildAttributes: ...
    , document
    }
```

The type signature for `buildWidget` looks like:

```purescript
buildWidget
  ∷ ∀ eff a
  . V.VDomSpec eff a MyWidget
  → V.VDomMachine eff MyWidget DOM.Node
```

`buildWidget` takes a circular reference to the `VDomSpec` you are building so you
can have recursive trees. The core though is in the returned `VDomMachine` which
takes your widget type, and yields a DOM node.

The type signature for `buildAttributes` looks like:

```purescript
buildAttributes
  ∷ ∀ eff
  . DOM.Element
  → V.VDomMachine eff MyAttribute Unit
```

This takes the current `Element` and yields a machine which takes your attribute
type and yields `Unit`.

If you don't have any custom widgets, you can supply a `Void` machine.

```purescript
import Halogen.VDom as VDom
import Halogen.VDom.Machine as Machine

data MyAttribute

makeSpec ∷ ∀ eff. DOM.Document → VDom.VDomSpec eff MyAttribute Void
makeSpec document =
  VDom.VDomSpec
    { buildWidget: const (Machine.never)
    , buildAttributes: ...
    , document
    }
```

## Creating Machines

A `Machine`'s type looks like:

```purescript
type Machine m a b = a → m (Step m a b)

data Step m a b = Step b (Machine m a b) (m Unit)
```

So it is just an effectful function from some input to a `Step`, which is a
product of an output value `b`, the next transition, and a finalizer. Finalizers
are useful when your widgets or attributes need to perform cleanup.

The structure of a widget machine will likely follow this pattern:

```purescript
import Halogen.VDom as V

createWidgetNode ∷ MyWidget → V.VDomEff eff DOM.Node

patchWidgetNode ∷ DOM.Node → MyWidget → MyWidget → V.VDomEff eff DOM.Node

cleanupWidgetNode ∷ DOM.Node → MyWidget → V.VDomEff eff Unit

buildWidget
  ∷ ∀ eff a
  . V.VDomSpec eff a MyWidget
  → V.VDomMachine eff MyWidget DOM.Node
buildWidget spec = render
  where
  render ∷ V.VDomMachine eff MyWidget DOM.Node
  render widget = do
    node ← createWidgetNode widget
    pure
      (V.Step node
        (patch node widget)
        (done node widget))

  patch ∷ DOM.Node → MyWidget → V.VDomMachine eff MyWidget DOM.Node
  patch node1 widget1 widget2 = do
    node2 ← patchWidgetNode node widget1 widget2
    pure
      (V.Step node2
        (patch node2 myWidget2)
        (done node2 myWidget2))

  done ∷ DOM.Node → MyWidget → V.VDomEff eff Unit
  done node widget = cleanupWidgetNode node widget
```

Note that `Machine`s can keep any state they need to, it is just passed from
machine to machine through closures.

The structure of an attribute machine will likely follow this pattern:

```purescript
import Halogen.VDom as V

applyAttributes ∷ DOM.Element → MyAttribute → V.VDomEff eff Unit

patchAttributes ∷ DOM.Element → MyAttribute → MyAttribute → V.VDomEff eff Unit

cleanupAttributes ∷ DOM.Element → MyAttribute → V.VDomEff eff Unit

buildAttributes
  ∷ ∀ eff a
  . DOM.Element
  → V.VDomMachine eff MyAttribute Unit
buildAttribute elem = apply
  where
  apply ∷ V.VDomMachine eff MyAttribute Unit
  apply attrs = do
    applyAttributes elem attrs
    pure
      (V.Step unit
        (patch attrs)
        (done attrs))

  patch ∷ MyAttribute → V.VDomMachine eff MyAttribute Unit
  patch attrs1 attrs2 = do
    patchAttributes elem attrs1 attrs2
    pure
      (V.Step unit
        (patch attrs2)
        (done attrs2))

  done ∷ MyAttribute → V.VDomEff eff Unit
  done attrs = cleanupAttribute elem attrs
```

Note that the `Element` is provided on initialization, and there is no meaninful
output type because it is only effectful.

## Getting Performance

The core of `Halogen.VDom` strives to be as fast as possible. It does this
through pervasive use of monomorphic `Eff` do-blocks (which are optimized into
imperative JavaScript) and `Data.Function.Uncurried` (which eliminates the
overhead of currying). It also provides a few monomorphic utilities in
`Halogen.VDom.Util` to help cut down on allocations. Additionally there are
some general purposes utilities to help with faster diffing.
