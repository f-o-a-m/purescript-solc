module Language.Solidity.Compiler
  ( compile
  ) where

import Prelude

import Data.Argonaut as A
import Data.Argonaut.Parser as AP
import Data.Either (Either(..))
import Data.Function.Uncurried (Fn2, runFn2)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Language.Solidity.Compiler.Types (CompilerInput, CompilerOutput)
import Node.Path (FilePath)

foreign import data SolcReadFileCallbackResult :: Type
foreign import callbackSuccess :: String -> SolcReadFileCallbackResult
foreign import callbackFailure :: String -> SolcReadFileCallbackResult
foreign import _compile :: Fn2 String (FilePath -> Effect SolcReadFileCallbackResult) (Effect String)

compile :: forall m.
           MonadEffect m
        => CompilerInput
        -> (FilePath -> Effect (Either String String))
        -> m (Either String CompilerOutput)
compile input readFile = do
  compilerOutput <- liftEffect $ runFn2 _compile inputJson liftedCallback
  pure (AP.jsonParser compilerOutput >>= A.decodeJson)

  where inputJson      = A.stringify (A.encodeJson input)
        liftedCallback path = do
          callbackResult <- readFile path
          pure $ case callbackResult of
            Left err  -> callbackFailure err
            Right res -> callbackSuccess res