{ name = "colors"
, dependencies =
  [ "arrays"
  , "assert"
  , "effect"
  , "console"
  , "control"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "math"
  , "maybe"
  , "partial"
  , "prelude"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
