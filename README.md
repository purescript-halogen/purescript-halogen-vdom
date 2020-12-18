# purescript-halogen-vdom

[![Latest release](http://img.shields.io/github/release/purescript-halogen/purescript-halogen-vdom.svg)](https://github.com/purescript-halogen/purescript-halogen-vdom/releases)
[![Build status](https://github.com/purescript-halogen/purescript-halogen-vdom/workflows/CI/badge.svg?branch=master)](https://github.com/purescript-halogen/purescript-halogen-vdom/actions?query=workflow%3ACI+branch%3Amaster)
[![Pursuit](https://pursuit.purescript.org/packages/purescript-halogen-vdom/badge)](https://pursuit.purescript.org/packages/purescript-halogen-vdom)

An extensible virtual-dom library for PureScript.

## Installation

Install with Spago:

```
spago install halogen-vdom
```

## Quick Start

You can get started with `halogen-vdom` with these resources:

- Read the [guide](./GUIDE.md).
- See the [test example](./test/Main.purs).

## Overview

`Halogen.VDom` is a bare-bones virtual-dom library for PureScript with inspiration drawn from:

- https://github.com/Matt-Esch/virtual-dom
- https://github.com/paldepind/snabbdom
- https://github.com/elm-lang/virtual-dom

Its goals include:

1. Use as little FFI as possible.
2. Be as fast as possible given (1).
3. Be extensible.

Notably, `Halogen.VDom` is largely useless out of the box. You'll need to bring your own attributes, properties, and event listeners (though there is a working implementation included). It is intended to be extended (and likely `newtype`d) by other frameworks to suit their needs.
