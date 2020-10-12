{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "solc"
, dependencies =
  [ "aff"
  , "argonaut"
  , "console"
  , "effect"
  , "node-path"
  , "prelude"
  , "psci-support"
  , "web3"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}
