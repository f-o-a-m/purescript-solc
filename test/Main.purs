module Test.Main where

import Prelude

import Data.Time.Duration (Milliseconds(..))
import Data.Either (Either(..), isRight)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Data.String (Pattern(..), stripPrefix)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff, error, throwError)
import Test.Spec (describe, it, parallel)
import Test.Spec.Assertions (shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess')
import Test.Spec.Runner.Node.Config (defaultConfig)

import Language.Solidity.Compiler as Compiler
import Language.Solidity.Compiler.Releases as Releases

knownCompilers :: Array { version :: String, remote :: String }
knownCompilers =
  [ { version: "0.4.26"
    , remote: "v0.4.26+commit.4563c3fc"
    }
  , { version: "0.5.17"
    , remote: "v0.5.17+commit.d19bba13"
    }
  , { version: "0.6.12"
    , remote: "v0.6.12+commit.27d51765"
    }
  , { version: "0.7.6"
    , remote: "v0.7.6+commit.7338295f"
    }
  , { version: "0.8.21"
    , remote: "v0.8.21+commit.d9974bed"
    }
  ]

compilerVersionMatches :: String -> String -> Boolean
compilerVersionMatches remote compiler = pred
  where
  cleanup s = fromMaybe s $ stripPrefix (Pattern "v") s
  cleanedCompiler = cleanup compiler
  cleanedRemote = cleanup remote
  pred = isJust $ stripPrefix (Pattern cleanedRemote) cleanedCompiler

main :: Effect Unit
main = do
  let
    cfg =
      { defaultConfig: defaultConfig { timeout = Just (Milliseconds $ 120.0 * 1000.0) }
      , parseCLIOptions: false
      }
  runSpecAndExitProcess' @Aff cfg [ consoleReporter ] do
    parallel $ describe "Releases" do
      it "can fetch the release list from the default repo" do
        rl <- Releases.getReleaseList Releases.defaultReleaseRepo
        rl `shouldSatisfy` isRight

      it "can fetch the latest release from the default repo" do
        source <- Releases.getReleaseSource Releases.defaultReleaseRepo "latest"
        source `shouldSatisfy` isRight

    parallel $ describe "Compiler" do
      it ("can read the version of the default compiler: " <> Compiler.version Compiler.defaultCompiler) do
        -- nb: this just shouldn't throw...
        pure unit

    parallel $ describe "Known versioned releases" do
      for_ knownCompilers $ \{ version, remote } -> do
        describe version do
          it "can fetch the compiler" do
            source <- Releases.getReleaseSource Releases.defaultReleaseRepo version
            source `shouldSatisfy` isRight

          it "can use loadRemoteVersion" do
            void $ Compiler.loadRemoteVersion remote

          it "can use the fetched compiler" do
            Releases.getReleaseSource Releases.defaultReleaseRepo version >>= case _ of
              Left err -> throwError $ error err
              Right source -> do
                compiler <- Compiler.useCompiler source
                Compiler.version compiler `shouldSatisfy` (compilerVersionMatches remote)
