{ name = "solc"
, dependencies =
  [ "aff"
  , "argonaut"
  , "argonaut-codecs"
  , "arrays"
  , "bifunctors"
  , "control"
  , "effect"
  , "either"
  , "eth-core"
  , "foldable-traversable"
  , "foreign-object"
  , "functions"
  , "integers"
  , "maybe"
  , "newtype"
  , "node-path"
  , "prelude"
  , "psci-support"
  , "strings"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
