{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "halogen-vdom"
, dependencies =
  [ "bifunctors"
  , "console"
  , "effect"
  , "exists"
  , "foreign"
  , "foreign-object"
  , "js-timers"
  , "maybe"
  , "prelude"
  , "psci-support"
  , "refs"
  , "tuples"
  , "unsafe-coerce"
  , "web-html"
  , "web-dom"
  , "debug"
  , "strings"
  , "control"
  , "lazy"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
