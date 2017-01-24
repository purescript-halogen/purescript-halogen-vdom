# purescript-halogen-vdom

An extensible virtual-dom library for PureScript.

## Overview

`Halogen.VDom` is a bare-bones virtual-dom library for PureScript with
inspiration drawn from:

* https://github.com/Matt-Esch/virtual-dom
* https://github.com/paldepind/snabbdom
* https://github.com/elm-lang/virtual-dom

It's goals being:

1. To use as little FFI as possible.
2. To be as fast as possible given (1).
3. To be extensible.

Notably, `Halogen.VDom` is largely useless out of the box. You'll need to bring
your own attributes, properties, and event listeners (though there is a working
implementation included). It is intended to be extended (and likely
`newtype`d) by other frameworks to suit their needs.

---

* Read the [guide](./GUIDE.md).
* See the [test example](./test/Main.purs).
