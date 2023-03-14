{ name = "halogen-vdom"
, dependencies = 
    [ "effect"
    , "prelude" 
    , "foreign-object"
    , "arrays"
    , "bifunctors"
    , "foreign"
    , "functions"
    , "maybe"
    , "newtype"
    , "nullable"
    , "refs"
    , "tuples"
    , "unsafe-coerce"
    , "web-dom"
    , "web-events"
    ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
