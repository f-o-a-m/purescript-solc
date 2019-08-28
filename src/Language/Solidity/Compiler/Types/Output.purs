module Language.Solidity.Compiler.Types.Output
  ( CompilerOutput
  ) where

import Prelude
import Data.Argonaut (class DecodeJson)
import Data.Argonaut as A

newtype CompilerOutput = CompilerOutput Unit

instance decodeJsonCompilerOutput :: DecodeJson CompilerOutput where
  decodeJson _ = pure (CompilerOutput unit)