{ name = "colors"
, dependencies =
  [ "arrays"
  , "assert"
  , "effect"
  , "console"
  , "either"
  , "foldable-traversable"
  , "integers"
  , "lists"
  , "math"
  , "maybe"
  , "partial"
  , "prelude"
  , "psci-support"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
