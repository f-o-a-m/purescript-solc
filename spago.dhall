{-
Welcome to a Spago project!
You can edit this file as you like.
-}
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
  , "strings"
  , "transformers"
  , "tuples"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
