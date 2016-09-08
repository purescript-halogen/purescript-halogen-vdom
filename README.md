# purescript-halogen-vdom

An extensible virtual-dom library for PureScript.

## Overview

`Halogen.VDom` is a "batteries not included" virtual-dom library for PureScript
with inspiration drawn from:

* https://github.com/Matt-Esch/virtual-dom
* https://github.com/paldepind/snabbdom
* https://github.com/elm-lang/virtual-dom

It's goals being:

1. To use as little FFI as possible.
2. To be as fast as possible given (1).
3. To be extensible.

Notably, `Halogen.VDom` is largely useless out of the box. It does not support
attributes, properties, or event listeners. It is intended to be extended
(and likely `newtype`d) by other frameworks to suit their needs.

---

* Read the [guide](./GUIDE.md).
* See the [test example](./test/Main.purs).
