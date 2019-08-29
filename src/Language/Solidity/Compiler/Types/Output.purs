module Language.Solidity.Compiler.Types.Output
  ( ErrorType(..)
  , ErrorSeverity(..)
  , SourceLocation(..)
  , CompilationError(..)
  , SourceLevelOutput(..)
  , LinkReference(..)
  , LinkReferences(..)
  , BytecodeOutput(..)
  , MethodIdentifiers(..)
  , GasEstimate(..)
  , GasEstimates(..)
  , EvmOutput(..)
  , EwasmOutput(..)
  , ContractLevelOutput(..)
  , CompilerOutput(..)
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, Json, decodeJson, (.!=), (.:), (.:?))
import Data.Argonaut as A
import Data.Either (Either(..), note)
import Data.Maybe (Maybe)
import Data.Int as Int
import Foreign.Object as FO
import Language.Solidity.Compiler.Types.Common (ContractMapped, FileMapped, Strung)
import Network.Ethereum.Types (HexString)
import Network.Ethereum.Core.BigNumber (BigNumber, parseBigNumber)

--------------------------------------------------
--- "errors[].type" field of output
data ErrorType = JSONError
               | IOError
               | ParserError
               | DocstringParsingError
               | SyntaxError
               | DeclarationError
               | TypeError
               | UnimplementedFeatureError
               | InternalCompilerError
               | Exception
               | CompilerError
               | FatalError
               | Warning

instance decodeJsonErrorType :: DecodeJson ErrorType where
  decodeJson j = decodeJson j >>= case _ of
    "JSONError" -> pure JSONError
    "IOError" -> pure IOError
    "ParserError" -> pure ParserError
    "DocstringParsingError" -> pure DocstringParsingError
    "SyntaxError" -> pure SyntaxError
    "DeclarationError" -> pure DeclarationError
    "TypeError" -> pure TypeError
    "UnimplementedFeatureError" -> pure UnimplementedFeatureError
    "InternalCompilerError" -> pure InternalCompilerError
    "Exception" -> pure Exception
    "CompilerError" -> pure CompilerError
    "FatalError" -> pure FatalError
    "Warning" -> pure Warning
    x -> Left $ "Unexpected ErrorType " <> x

--------------------------------------------------
--- "errors[].severity" field of output

data ErrorSeverity = SeverityError | SeverityWarning

instance decodeJsonErrorSeverity :: DecodeJson ErrorSeverity where
  decodeJson o = decodeJson o >>= case _ of
    "error"   -> pure SeverityError
    "warning" -> pure SeverityWarning
    x         -> Left $ "Unexpected ErrorSeverity " <> x

--------------------------------------------------
--- "errors[].sourceLocation" and ".secondarySourceLocations" field of output
newtype SourceLocation = SourceLocation
  { file    :: String
  , start   :: Int
  , end     :: Int
  , message :: Maybe String
  }

instance decodeJsonSourceLocation :: DecodeJson SourceLocation where
  decodeJson j = do
    o <- decodeJson j
    file    <- o .: "file"
    start   <- o .: "start"
    end     <- o .: "end"
    message <- o .:? "message"
    pure $ SourceLocation { file, start, end, message }

--------------------------------------------------
--- "errors" field of output

newtype CompilationError = CompilationError
  { type :: ErrorType
  , component :: String
  , severity :: ErrorSeverity
  , message :: String
  , formattedMessage :: Maybe String
  , sourceLocation :: Maybe SourceLocation
  , secondarySourceLocations :: Array SourceLocation
  }

instance decodeJsonCompilationError :: DecodeJson CompilationError where
  decodeJson j = do
    o <- decodeJson j
    ty                       <- o .: "type"
    component                <- o .: "component"
    severity                 <- o .: "severity"
    message                  <- o .: "message"
    formattedMessage         <- o .:? "formattedMessage"
    sourceLocation           <- o .:? "sourceLocation"
    secondarySourceLocations <- o .:? "secondarySourceLocations" .!= []
    pure $ CompilationError 
      { type: ty
      , component
      , severity
      , message
      , formattedMessage
      , sourceLocation
      , secondarySourceLocations
      }

--------------------------------------------------
--- "sources{}" field of output

newtype SourceLevelOutput = SourceLevelOutput
  { id :: Int
  , ast :: Maybe A.Json
  , legacyAST :: Maybe A.Json
  }

instance decodeJsonSourceLevelOutput :: DecodeJson SourceLevelOutput where
  decodeJson j = do
    o <- decodeJson j
    id <- o .: "id"
    ast <- o .:? "ast"
    legacyAST <- o .:? "legacyAST"
    pure $ SourceLevelOutput { id, ast, legacyAST }

--------------------------------------------------
--- "contracts{}{}.evm.{deployedBytecode, bytecode}.linkReferences" field of output

data LinkReference = LinkReference
  { start  :: Int
  , length :: Int
  }

instance decodeJsonLinkReference :: DecodeJson LinkReference where
  decodeJson j = do
    o <- decodeJson j
    start  <- o .: "start"
    length <- o .: "length"
    pure $ LinkReference { start, length }

newtype LinkReferences = LinkReferences (FileMapped (ContractMapped (Array LinkReference)))
derive newtype instance decodeJsonLinkReferences :: DecodeJson LinkReferences

--------------------------------------------------
--- "contracts{}{}.evm.{deployedBytecode, bytecode}" field of output
newtype BytecodeOutput = BytecodeOutput
  { object :: Maybe HexString
  , opcodes :: Maybe String
  , sourceMapping :: Maybe (Strung Json)
  , linkReferences :: Maybe LinkReferences
  }

instance decodeJsonBytecodeOutput :: DecodeJson BytecodeOutput where
  decodeJson j = do
    o <- decodeJson j
    object <- o .:? "object"
    opcodes <- o .:? "opcodes"
    sourceMapping <- o .:? "sourceMapping"
    linkReferences <- o .:? "linkReferences"
    pure $ BytecodeOutput { object, opcodes, sourceMapping, linkReferences }

--------------------------------------------------
--- "contracts{}{}.evm.methodIdentifiers" field of output
newtype MethodIdentifiers = MethodIdentifiers (FO.Object HexString)

derive newtype instance decodeJsonMethodIdentifiers :: DecodeJson MethodIdentifiers

--------------------------------------------------
--- "contracts{}{}.evm.gasEstimates.*" values of output

data GasEstimate = InfiniteGas | GasCount BigNumber

instance decodeJsonGasEstimate :: DecodeJson GasEstimate where
  decodeJson j = decodeJson j >>= case _ of
    "infinite" -> pure InfiniteGas
    x -> note "invalid BigNumber" $ GasCount <$> parseBigNumber Int.decimal x

newtype GasEstimates = GasEstimates (FO.Object GasEstimate)
derive newtype instance decodeJsonGasEstimates :: DecodeJson GasEstimates

newtype CreationGasEstimates = CreationGasEstimates
  { codeDepositCost :: Maybe GasEstimate
  , executionCost   :: Maybe GasEstimate
  , totalCost       :: Maybe GasEstimate
  }

instance decodeJsonCreationGasEstimates :: DecodeJson CreationGasEstimates where
  decodeJson j = do
    o <- decodeJson j
    codeDepositCost <- o .:? "codeDepositCost"
    executionCost   <- o .:? "executionCost"
    totalCost       <- o .:? "totalCost"
    pure $ CreationGasEstimates { codeDepositCost, executionCost, totalCost }

--------------------------------------------------
--- "contracts{}{}.evm.gasEstimates" field of output
newtype ContractGasEstimates = ContractGasEstimates
  { creation :: Maybe CreationGasEstimates
  , external :: Maybe (FO.Object GasEstimate)
  , internal :: Maybe (FO.Object GasEstimate)
  }

instance decodeJsonContractGasEstimates :: DecodeJson ContractGasEstimates where
  decodeJson j = do
    o <- decodeJson j
    creation <- o .:? "creation"
    external <- o .:? "external"
    internal <- o .:? "internal"
    pure $ ContractGasEstimates { creation, external, internal }

--------------------------------------------------
--- "contracts{}{}.evm" field of output

newtype EvmOutput = EvmOutput
  { assembly :: Maybe String
  , legacyAssembly :: Maybe Json
  , bytecode :: Maybe BytecodeOutput
  , deployedBytecode :: Maybe BytecodeOutput
  , methodIdentifiers :: Maybe MethodIdentifiers
  , gasEstimates :: Maybe GasEstimates
  }

instance decodeJsonEvmOutput :: DecodeJson EvmOutput where
  decodeJson j = do
    o <- decodeJson j
    assembly <- o .:? "assembly"
    legacyAssembly <- o .:? "legacyAssembly"
    bytecode <- o .:? "bytecode"
    deployedBytecode <- o .:? "deployedBytecode"
    methodIdentifiers <- o .:? "methodIdentifiers"
    gasEstimates <- o .:? "gasEstimates"
    pure $ EvmOutput
      { assembly
      , legacyAssembly
      , bytecode
      , deployedBytecode
      , methodIdentifiers
      , gasEstimates
      }

--------------------------------------------------
--- "contracts{}{}.ewasm" field of output
newtype EwasmOutput = EwasmOutput
  { wast :: Maybe String
  , wasm :: Maybe HexString
  }

instance decodeJsonEwasmOutput :: DecodeJson EwasmOutput where
  decodeJson j = do
    o <- decodeJson j
    wast <- o .:? "wast"
    wasm <- o .:? "wasm"
    pure $ EwasmOutput { wast, wasm }

--------------------------------------------------
--- "contracts{}{}" field of output

newtype ContractLevelOutput = ContractLevelOutput
  { abi :: A.Json
  , metadata :: Maybe (Strung A.Json)
  , userdoc :: Maybe A.Json
  , devdoc :: Maybe A.Json
  , ir :: Maybe String
  , evm :: Maybe EvmOutput
  , ewasm :: Maybe EwasmOutput
  }

instance decodeJsonContractLevelOutput :: DecodeJson ContractLevelOutput where
  decodeJson j = do
    o <- decodeJson j
    abi <- o .: "abi"
    metadata <- o .:? "metadata"
    userdoc <- o .:? "userdoc"
    devdoc <- o .:? "devdoc"
    ir <- o .:? "ir"
    evm <- o .:? "evm"
    ewasm <- o .:? "ewasm"
    pure $ ContractLevelOutput { abi, metadata, userdoc, devdoc, ir, evm, ewasm }

--------------------------------------------------
--- the compiler output
newtype CompilerOutput = CompilerOutput
  { errors :: Array CompilationError
  , sources :: FileMapped SourceLevelOutput
  , contracts :: FileMapped (ContractMapped ContractLevelOutput)
  }

instance decodeJsonCompilerOutput :: DecodeJson CompilerOutput where
  decodeJson j = do
    o <- decodeJson j
    errors    <- o .:? "errors" .!= []
    sources   <- o .: "sources"
    contracts <- o .: "contracts"
    pure $ CompilerOutput { errors, sources, contracts }