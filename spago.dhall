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
  , "maybe"
  , "partial"
  , "prelude"
  , "strings"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
