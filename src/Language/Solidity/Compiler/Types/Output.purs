module Language.Solidity.Compiler.Types.Output
  ( ErrorType(..)
  , ErrorSeverity(..)
  , SourceLocation(..)
  , CompilationError(..)
  , SourceLevelOutput(..)
  , BytecodeObject(..)
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
  , mkBytecodeObject
  , unBytecodeObject
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, jsonEmptyObject, (.!=), (.:), (.:?), (~>), (:=))
import Data.Argonaut as A
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Either (Either(..), note)
import Data.Int as Int
import Data.Maybe (Maybe, maybe)
import Data.Newtype (class Newtype)
import Foreign.Object as FO
import Language.Solidity.Compiler.Types.Common (ContractMapped, FileMapped, Strung)
import Network.Ethereum.Core.BigNumber (BigNumber, parseBigNumber)
import Network.Ethereum.Types (HexString, mkHexString, unHex)

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
derive instance eqErrorType  :: Eq ErrorType
derive instance ordErrorType :: Ord ErrorType

instance showErrorType :: Show ErrorType where
  show JSONError                 = "JSON Error"
  show IOError                   = "IO Error"
  show ParserError               = "Parser Error"
  show DocstringParsingError     = "Docstring Parsing Error"
  show SyntaxError               = "Syntax Error"
  show DeclarationError          = "Declaration Error"
  show TypeError                 = "Type Error"
  show UnimplementedFeatureError = "Unimplemented Feature Error"
  show InternalCompilerError     = "Internal Compiler Error"
  show Exception                 = "Compiler Exception"
  show CompilerError             = "Compiler Error"
  show FatalError                = "Fatal Error"
  show Warning                   = "Warning"

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
    x -> Left $ Named ("Unexpected ErrorType " <> x) $ UnexpectedValue j

--------------------------------------------------
--- "errors[].severity" field of output

data ErrorSeverity = SeverityError | SeverityWarning
derive instance eqErrorSeverity  :: Eq ErrorSeverity
derive instance ordErrorSeverity :: Ord ErrorSeverity

instance decodeJsonErrorSeverity :: DecodeJson ErrorSeverity where
  decodeJson o = decodeJson o >>= case _ of
    "error"   -> pure SeverityError
    "warning" -> pure SeverityWarning
    x         -> Left $ Named ("Unexpected ErrorSeverity " <> x) $ UnexpectedValue o

--------------------------------------------------
--- "errors[].sourceLocation" and ".secondarySourceLocations" field of output
newtype SourceLocation = SourceLocation
  { file    :: String
  , start   :: Int
  , end     :: Int
  , message :: Maybe String
  }
derive instance eqSourceLocation :: Eq SourceLocation
derive instance ordSourceLocation :: Ord SourceLocation

instance showSourceLocation :: Show SourceLocation where
  show (SourceLocation sl) =
    let msg = maybe "" (append ": ") sl.message
    in sl.file <> ":" <> show sl.start <> "-" <> show sl.end <> msg

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

data CompilationError = SimpleCompilationError String
                      | FullCompilationError
                        { type :: ErrorType
                        , component :: String
                        , severity :: ErrorSeverity
                        , message :: String
                        , formattedMessage :: Maybe String
                        , sourceLocation :: Maybe SourceLocation
                        , secondarySourceLocations :: Array SourceLocation
                        }
derive instance eqCompilationError  :: Eq CompilationError
derive instance ordCompilationError :: Ord CompilationError

instance decodeJsonCompilationError :: DecodeJson CompilationError where
  decodeJson j = (SimpleCompilationError <$> decodeJson j) <|> (decodeAsObject =<< decodeJson j)
    where decodeAsObject o = do
            ty                       <- o .: "type"
            component                <- o .: "component"
            severity                 <- o .: "severity"
            message                  <- o .: "message"
            formattedMessage         <- o .:? "formattedMessage"
            sourceLocation           <- o .:? "sourceLocation"
            secondarySourceLocations <- o .:? "secondarySourceLocations" .!= []
            pure $ FullCompilationError
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
derive instance eqSourceLevelOutput  :: Eq SourceLevelOutput
derive instance ordSourceLevelOutput :: Ord SourceLevelOutput

instance decodeJsonSourceLevelOutput :: DecodeJson SourceLevelOutput where
  decodeJson j = do
    o <- decodeJson j
    id <- o .: "id"
    ast <- o .:? "ast"
    legacyAST <- o .:? "legacyAST"
    pure $ SourceLevelOutput { id, ast, legacyAST }

--------------------------------------------------
--- "contracts{}{}.evm.{deployedBytecode, bytecode}.object" field of output
data BytecodeObject = BytecodeHexString HexString | BytecodeUnlinked String
derive instance eqBytecodeObject  :: Eq BytecodeObject
derive instance ordBytecodeObject :: Ord BytecodeObject

instance decodeJsonBytecodeObject :: DecodeJson BytecodeObject where
  decodeJson = map mkBytecodeObject <<< decodeJson

instance encodeJsonBytecodeObject :: EncodeJson BytecodeObject where
  encodeJson = encodeJson <<< unBytecodeObject

mkBytecodeObject :: String -> BytecodeObject
mkBytecodeObject s = maybe (BytecodeUnlinked s) BytecodeHexString (mkHexString s)

unBytecodeObject :: BytecodeObject -> String
unBytecodeObject (BytecodeHexString s) = unHex s
unBytecodeObject (BytecodeUnlinked u)  = u

--------------------------------------------------
--- "contracts{}{}.evm.{deployedBytecode, bytecode}.linkReferences" field of output

data LinkReference = LinkReference
  { start  :: Int
  , length :: Int
  }
derive instance eqLinkReference  :: Eq LinkReference
derive instance ordLinkReference :: Ord LinkReference

instance decodeJsonLinkReference :: DecodeJson LinkReference where
  decodeJson j = do
    o <- decodeJson j
    start  <- o .: "start"
    length <- o .: "length"
    pure $ LinkReference { start, length }

instance encodeJsonLinkReference :: EncodeJson LinkReference where
  encodeJson (LinkReference { start, length }) =
       "start"  := start
    ~> "length" := length
    ~> jsonEmptyObject

newtype LinkReferences = LinkReferences (FileMapped (ContractMapped (Array LinkReference)))
derive instance newtypeLinkReferences :: Newtype LinkReferences _
derive newtype instance eqLinkReferences  :: Eq LinkReferences
derive newtype instance ordLinkReferences :: Ord LinkReferences
derive newtype instance decodeJsonLinkReferences :: DecodeJson LinkReferences

--------------------------------------------------
--- "contracts{}{}.evm.{deployedBytecode, bytecode}" field of output
newtype BytecodeOutput = BytecodeOutput
  { object :: Maybe BytecodeObject
  , opcodes :: Maybe String
  , sourceMapping :: Maybe (Strung Json)
  , linkReferences :: Maybe LinkReferences
  }
derive instance eqBytecodeOutput  :: Eq BytecodeOutput
derive instance ordBytecodeOutput :: Ord BytecodeOutput

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
derive instance newtypeMethodIdentifiers :: Newtype MethodIdentifiers _
derive newtype instance eqMethodIdentifiers  :: Eq MethodIdentifiers
derive newtype instance ordMethodIdentifiers :: Ord MethodIdentifiers
derive newtype instance decodeJsonMethodIdentifiers :: DecodeJson MethodIdentifiers

--------------------------------------------------
--- "contracts{}{}.evm.gasEstimates.*" values of output

data GasEstimate = InfiniteGas | GasCount BigNumber
derive instance eqGasEstimate  :: Eq GasEstimate
derive instance ordGasEstimate :: Ord GasEstimate

instance decodeJsonGasEstimate :: DecodeJson GasEstimate where
  decodeJson j = decodeJson j >>= case _ of
    "infinite" -> pure InfiniteGas
    x -> note (Named "invalid BigNumber" $ UnexpectedValue j) $ GasCount <$> parseBigNumber Int.decimal x

newtype GasEstimates = GasEstimates (FO.Object GasEstimate)
derive newtype instance eqGasEstimates :: Eq GasEstimates
derive newtype instance ordGasEstimates :: Ord GasEstimates
derive newtype instance decodeJsonGasEstimates :: DecodeJson GasEstimates

newtype CreationGasEstimates = CreationGasEstimates
  { codeDepositCost :: Maybe GasEstimate
  , executionCost   :: Maybe GasEstimate
  , totalCost       :: Maybe GasEstimate
  }
derive instance eqCreationGasEstimates  :: Eq CreationGasEstimates
derive instance ordCreationGasEstimates :: Ord CreationGasEstimates

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
derive instance eqContractGasEstimates :: Eq ContractGasEstimates
derive instance ordContractGasEstimates :: Ord ContractGasEstimates

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
derive instance eqEvmOutput :: Eq EvmOutput
derive instance ordEvmOutput :: Ord EvmOutput

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
derive instance eqEwasmOutput :: Eq EwasmOutput
derive instance ordEwasmOutput :: Ord EwasmOutput

instance decodeJsonEwasmOutput :: DecodeJson EwasmOutput where
  decodeJson j = do
    o <- decodeJson j
    wast <- o .:? "wast"
    wasm <- o .:? "wasm"
    pure $ EwasmOutput { wast, wasm }

--------------------------------------------------
--- "contracts{}{}" field of output

newtype ContractLevelOutput = ContractLevelOutput
  { abi :: Maybe A.Json
  , metadata :: Maybe (Strung A.Json)
  , userdoc :: Maybe A.Json
  , devdoc :: Maybe A.Json
  , ir :: Maybe String
  , evm :: Maybe EvmOutput
  , ewasm :: Maybe EwasmOutput
  }
derive instance eqContractLevelOutput  :: Eq ContractLevelOutput
derive instance ordContractLevelOutput :: Ord ContractLevelOutput

instance decodeJsonContractLevelOutput :: DecodeJson ContractLevelOutput where
  decodeJson j = do
    o <- decodeJson j
    abi <- o .:? "abi"
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
derive instance eqCompilerOutput  :: Eq CompilerOutput
derive instance ordCompilerOutput :: Ord CompilerOutput

instance decodeJsonCompilerOutput :: DecodeJson CompilerOutput where
  decodeJson j = do
    o <- decodeJson j
    errors    <- o .:? "errors" .!= []
    sources   <- o .:? "sources" .!= FO.empty
    contracts <- o .:? "contracts" .!= FO.empty
    pure $ CompilerOutput { errors, sources, contracts }
