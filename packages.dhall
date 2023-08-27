let upstream =
      https://github.com/purescript/package-sets/releases/download/psc-0.15.7-20230306/packages.dhall
        sha256:0757626c7422b8b5b5b1d0df3d3628e5deac755d7f89c433a9bf89009787dcbd

let overrides = {=}

let additions =
      { bytestrings =
        { dependencies =
          [ "arrays"
          , "console"
          , "effect"
          , "exceptions"
          , "foldable-traversable"
          , "integers"
          , "leibniz"
          , "maybe"
          , "newtype"
          , "node-buffer"
          , "partial"
          , "prelude"
          , "quickcheck"
          , "quickcheck-laws"
          , "quotient"
          , "unsafe-coerce"
          ]
        , repo =
            "https://github.com/rightfold/purescript-bytestrings"
        , version = "6733a32fca306015b3428e9985ffac65325a9864"
        }
      , web3 =
        { dependencies =
          [ "aff"
          , "coroutines"
          , "coroutine-transducers"
          , "effect"
          , "errors"
          , "eth-core"
          , "foreign"
          , "fork"
          , "heterogeneous"
          , "parsing"
          , "partial"
          , "profunctor-lenses"
          , "tagged"
          , "transformers"
          , "typelevel-prelude"
          , "variant"
          , "argonaut"
          , "argonaut-generic"
          , "arrays"
          , "bifunctors"
          , "bytestrings"
          , "control"
          , "either"
          , "exceptions"
          , "foldable-traversable"
          , "foreign-object"
          , "integers"
          , "maybe"
          , "newtype"
          , "parallel"
          , "prelude"
          , "record"
          , "ring-modules"
          , "simple-json"
          , "strings"
          , "tailrec"
          , "tuples"
          , "unfoldable"
          ]
        , repo = "https://github.com/f-o-a-m/purescript-web3"
        , version = "dd47c28a3b8adc0167e43615f2ac27f48f23e59b"
        }
      , eth-core =
        { dependencies =
          [ "argonaut"
          , "arrays"
          , "bytestrings"
          , "effect"
          , "either"
          , "foreign"
          , "functions"
          , "integers"
          , "maybe"
          , "node-buffer"
          , "ordered-collections"
          , "partial"
          , "prelude"
          , "quotient"
          , "ring-modules"
          , "simple-json"
          , "strings"
          , "unsafe-coerce"
          ]
        , repo = "https://github.com/f-o-a-m/purescript-eth-core.git"
        , version = "b35eec551db445cb6a3577eaea3fe7a3bc052472"
        }
      , coroutine-transducers =
        { dependencies =
            [ "aff"
            , "coroutines"
            , "effect"
            , "maybe"
            , "psci-support"
            ]
        , repo =
            "https://github.com/blinky3713/purescript-coroutine-transducers"
        , version =
            "v1.0.0"
        }
      , quotient =
        { dependencies = [ "prelude", "quickcheck" ]
        , repo = "https://github.com/rightfold/purescript-quotient.git"
        , version = "v3.0.0"
        }
      , tagged =
        { dependencies =
            [ "identity"
            , "profunctor"
            ]
        , repo =
            "https://github.com/kejace/purescript-tagged"
        , version =
            "v0.14"
        }
      }

in  upstream // overrides // additions
