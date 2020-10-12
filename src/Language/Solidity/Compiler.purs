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
import Data.Function.Uncurried (Fn3, runFn3)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Effect.Class (class MonadEffect, liftEffect)
import Language.Solidity.Compiler.Types (CompilerInput, CompilerOutput)
import Node.Path (FilePath)

foreign import data SolcReadFileCallbackResult :: Type
foreign import data SolidityCompiler :: Type
foreign import callbackSuccess :: String -> SolcReadFileCallbackResult
foreign import callbackFailure :: String -> SolcReadFileCallbackResult
foreign import defaultCompiler :: SolidityCompiler
foreign import version :: SolidityCompiler -> String
foreign import useCompiler :: String -> SolidityCompiler
foreign import _loadRemoteVersion :: String -> EffectFnAff SolidityCompiler
foreign import _compile :: Fn3 SolidityCompiler Json (FilePath -> Effect SolcReadFileCallbackResult) (Effect Json)

compile
  :: forall m
   . MonadEffect m
  => SolidityCompiler
  -> CompilerInput
  -> (FilePath -> Effect (Either String String))
  -> m (Either String CompilerOutput)
compile solc input readFile = liftEffect $ map (lmap printJsonDecodeError) $
  A.decodeJson <$> runFn3 _compile solc (encodeJson input) liftedCallback

  where inputJson      = A.stringify (A.encodeJson input)
        liftedCallback = map (either callbackFailure callbackSuccess) <<< readFile

loadRemoteVersion
  :: forall m
   . MonadAff m
  => String
  -> m SolidityCompiler
loadRemoteVersion = liftAff <<< fromEffectFnAff <<< _loadRemoteVersion
