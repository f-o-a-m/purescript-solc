module Language.Solidity.Compiler
  ( SolidityCompiler
  , defaultCompiler
  , compile
  , loadRemoteVersion
  , useCompiler
  , version
  ) where

import Prelude

import Data.Argonaut (Json, encodeJson)
import Data.Argonaut as A
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either, either)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (class MonadEffect, liftEffect)
import Language.Solidity.Compiler.Types (CompilerInput, CompilerOutput)
import Node.Path (FilePath)

type ReadFileSuccess = String
type ReadFileError = String

foreign import data SolcReadFileCallbackResult :: Type
foreign import data SolidityCompiler :: Type
foreign import callbackSuccess :: ReadFileSuccess -> SolcReadFileCallbackResult
foreign import callbackFailure :: ReadFileError -> SolcReadFileCallbackResult
foreign import defaultCompiler :: SolidityCompiler
foreign import version :: SolidityCompiler -> String
foreign import useCompiler :: String -> SolidityCompiler -- takes output of `getReleaseSource` (the javascript code)
foreign import _loadRemoteVersion :: String -> EffectFnAff SolidityCompiler
foreign import _compile :: EffectFn3 SolidityCompiler Json (FilePath -> Effect SolcReadFileCallbackResult) Json

compile
  :: forall m
   . MonadEffect m
  => SolidityCompiler
  -> CompilerInput
  -> (FilePath -> Effect (Either ReadFileError ReadFileSuccess)) -- Example: this function ...
  -> m (Either String CompilerOutput)
compile solc input readFile = liftEffect $ map (lmap printJsonDecodeError) $
  A.decodeJson <$> runEffectFn3 _compile solc (encodeJson input) liftedCallback

  where
  liftedCallback = map (either callbackFailure callbackSuccess) <<< readFile

loadRemoteVersion
  :: forall m
   . MonadAff m
  => String
  -> m SolidityCompiler
loadRemoteVersion = liftAff <<< fromEffectFnAff <<< _loadRemoteVersion
