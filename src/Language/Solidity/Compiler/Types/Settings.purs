module Language.Solidity.Compiler.Types.Settings 
  ( class IsSelection
  , Remapping(..)
  , CompilerSettings(..)
  , OptimizerDetails(..)
  , YulOptimizerDetails(..)
  , OptimizerSettings(..)
  , EvmVersion(..)
  , MetadataSettings(..)
  , Library(..)
  , Libraries(..)
  , FileLevelSelection(..)
  , EvmBytecodeOutput(..)
  , EvmOutputSelection(..)
  , EwasmOutputSelection(..)
  , ContractLevelSelection(..)
  , OutputSelection(..)
  , OutputSelections(..)
  , decodeJsonSelection
  , encodeJsonSelection
  , fromSelection
  , toSelection
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson, fromString, jsonEmptyArray, jsonEmptyObject, jsonSingletonObject, (.!=), (.:), (.:!), (:=?), (~>?))
import Data.Argonaut as A
import Data.Argonaut.Decode.Error (JsonDecodeError(..), printJsonDecodeError)
import Data.Array (nub, null, uncons)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..), joinWith, split)
import Data.Traversable (for, traverse)
import Data.Tuple (Tuple(..))
import Foreign.Object as FO
import Language.Solidity.Compiler.Types.Common (ContractMapped, FileMapped, flattenOptionalArray)
import Network.Ethereum.Types (Address)
import Node.Path (FilePath)

--------------------------------------------------
--- "remappings" field of "settings" field
--- NB: this is a single remapping

data Remapping = GlobalRemapping { to :: FilePath}
               | Remapping { from :: FilePath, to :: FilePath }

derive instance eqRemapping  :: Eq Remapping
derive instance ordRemapping :: Ord Remapping

instance encodeJsonRemapping :: EncodeJson Remapping where
  encodeJson = A.fromString <<< case _ of
    GlobalRemapping g -> ":g=" <> g.to
    Remapping r       -> r.from <> "=" <> r.to

--------------------------------------------------
--- "settings.optimizer.yulDetails"

newtype YulOptimizerDetails = YulOptimizerDetails
  { stackAllocation :: Boolean
  }

derive instance eqYulOptimizerDetails :: Eq YulOptimizerDetails
derive instance ordYulOptimizerDetails :: Ord YulOptimizerDetails

instance decodeJsonYulOptimizerDetails :: DecodeJson YulOptimizerDetails where
  decodeJson j = do
    o <- decodeJson j
    stackAllocation <- o .: "stackAllocation"
    pure $ YulOptimizerDetails { stackAllocation }

instance encodeJsonYulOptimizerDetails :: EncodeJson YulOptimizerDetails where
  encodeJson (YulOptimizerDetails y) =
    jsonSingletonObject "stackAllocation" (A.fromBoolean y.stackAllocation)

--------------------------------------------------
--- "settings.optimizer.details"

newtype OptimizerDetails = OptimizerDetails
  { peephole          :: Maybe Boolean
  , jumpdestRemove    :: Maybe Boolean
  , orderLiterals     :: Maybe Boolean
  , deduplicate       :: Maybe Boolean
  , cse               :: Maybe Boolean
  , constantOptimizer :: Maybe Boolean
  , yul               :: Maybe Boolean
  , yulDetails        :: Maybe YulOptimizerDetails
  }

derive instance eqOptimizerDetails :: Eq OptimizerDetails
derive instance ordOptimizerDetails :: Ord OptimizerDetails

instance decodeJsonOptimizerDetails :: DecodeJson OptimizerDetails where
  decodeJson j = do
    o <- decodeJson j
    peephole          <- o .:! "peephole"
    jumpdestRemove    <- o .:! "jumpdestRemove"
    orderLiterals     <- o .:! "orderLiterals"
    deduplicate       <- o .:! "deduplicate"
    cse               <- o .:! "cse"
    constantOptimizer <- o .:! "constantOptimizer"
    yul               <- o .:! "yul"
    yulDetails        <- o .:! "yulDetails"
    pure $ OptimizerDetails
      { peephole
      , jumpdestRemove
      , orderLiterals
      , deduplicate
      , cse
      , constantOptimizer
      , yul
      , yulDetails
      }

instance encodeJsonOptimizerDetails :: EncodeJson OptimizerDetails where
  encodeJson (OptimizerDetails o) =
        "peephole"          :=? o.peephole
    ~>? "jumpdestRemove"    :=? o.jumpdestRemove
    ~>? "orderLiterals"     :=? o.orderLiterals
    ~>? "deduplicate"       :=? o.deduplicate
    ~>? "cse"               :=? o.cse
    ~>? "constantOptimizer" :=? o.constantOptimizer
    ~>? "yul"               :=? o.yul
    ~>? "yulDetails"        :=? o.yulDetails
    ~>? jsonEmptyObject

--------------------------------------------------
--- "settings.optimizer"

newtype OptimizerSettings = OptimizerSettings
  { enabled :: Maybe Boolean
  , runs    :: Maybe Int
  , details :: Maybe OptimizerDetails
  }

derive instance eqOptimizerSettings :: Eq OptimizerSettings
derive instance ordOptimizerSettings :: Ord OptimizerSettings

instance decodeJsonOptimizerSettings :: DecodeJson OptimizerSettings where
  decodeJson j = do
    o <- decodeJson j
    enabled <- o .:! "enabled"
    runs    <- o .:! "runs"
    details <- o .:! "details"
    pure $ OptimizerSettings { enabled, runs, details }

instance encodeJsonOptimizerSettings :: EncodeJson OptimizerSettings where
  encodeJson (OptimizerSettings o) =
        "enabled" :=? o.enabled
    ~>? "runs"    :=? o.runs
    ~>? "details" :=? o.details
    ~>? jsonEmptyObject

--------------------------------------------------
--- "settings.evmVersion"

data EvmVersion = Homestead        -- at 1150000
                | TangerineWhistle -- aka EIP-150, at 2463000
                | SpuriousDragon   -- aka EIP-607, at 2675000
                | Byzantium        -- aka EIP-609, at 4370000
                | Constantinople   -- aka EIP-1013, at 7280000
                | Petersburg       -- aka EIP-1014, at 7280000

derive instance eqEvmVersion :: Eq EvmVersion
derive instance ordEvmVersion :: Ord EvmVersion

instance decodeJsonEvmVersion :: DecodeJson EvmVersion where
  decodeJson j = decodeJson j >>= case _ of
    "homestead"        -> pure Homestead
    "tangerineWhistle" -> pure TangerineWhistle
    "spuriousDragon"   -> pure SpuriousDragon
    "byzantium"        -> pure Byzantium
    "constantinople"   -> pure Constantinople
    "petersburg"       -> pure Petersburg
    x                  -> Left $ Named ("Unknown EVM version " <> x) $ UnexpectedValue j

instance encodeJsonEvmVersion :: EncodeJson EvmVersion where
  encodeJson = A.fromString <<< case _ of
    Homestead        -> "homestead"
    TangerineWhistle -> "tangerineWhistle"
    SpuriousDragon   -> "spuriousDragon"
    Byzantium        -> "byzantium"
    Constantinople   -> "constantinople"
    Petersburg       -> "petersburg"

--------------------------------------------------
--- "settings.metadata"

newtype MetadataSettings = MetadataSettings
  { useLiteralContent :: Boolean
  }

derive instance eqMetadataSettings  :: Eq MetadataSettings
derive instance ordMetadataSettings :: Ord MetadataSettings

instance decodeJsonMetadataSettings :: DecodeJson MetadataSettings where
  decodeJson j = do
    o <- decodeJson j
    useLiteralContent <- o .:! "useLiteralContent" .!= false
    pure $ MetadataSettings { useLiteralContent }

instance encodeJsonMetadataSettings :: EncodeJson MetadataSettings where
  encodeJson (MetadataSettings ms) = 
    jsonSingletonObject "useLiteralContent" $ A.fromBoolean ms.useLiteralContent

--------------------------------------------------
--- "settings.libraries"

newtype Library = Library
  { libraryName :: String
  , address     :: Address
  }
derive instance eqLibrary :: Eq Library
derive instance ordLibrary :: Ord Library

instance encodeJsonLibrary :: EncodeJson Library where
  encodeJson (Library l) =
    jsonSingletonObject l.libraryName (encodeJson l.address)

newtype Libraries = Libraries (FileMapped Library)
derive newtype instance eqLibraries :: Eq Libraries
derive newtype instance ordLibraries :: Ord Libraries
derive newtype instance encodeJsonLibraries :: EncodeJson Libraries

--------------------------------------------------
--- "settings.outputSelection"

class IsSelection a where
  toSelection   :: a -> Array String
  fromSelection :: Array String -> Maybe a

decodeJsonSelection :: forall a. IsSelection a => Json -> Either String a
decodeJsonSelection j = do
  s <- lmap printJsonDecodeError $ decodeJson j
  let splits = split (Pattern ".") s
      sels   = fromSelection splits
  note ("Unknown output selection \"" <> s <> "\"") sels

decodeJsonSelection' :: forall a. IsSelection a => Json -> Either JsonDecodeError a
decodeJsonSelection' j = lmap (\e -> Named e $ UnexpectedValue j) $ decodeJsonSelection j

encodeJsonSelection :: forall a. IsSelection a => a -> Json
encodeJsonSelection = fromString <<< joinWith "." <<< toSelection

mapFromSelectionNullable :: forall a b.  IsSelection a => (Maybe a -> b) -> Array String -> Maybe b
mapFromSelectionNullable f [] = Just (f Nothing)
mapFromSelectionNullable f xs = (f <<< Just) <$> fromSelection xs

instance isSelectionMaybe :: IsSelection a => IsSelection (Maybe a) where
  toSelection Nothing  = []
  toSelection (Just a) = toSelection a

  fromSelection [] = Nothing
  fromSelection xs = fromSelection xs

data FileLevelSelection = AST
                        | LegacyAST
derive instance eqFileLevelSelection  :: Eq FileLevelSelection
derive instance ordFileLevelSelection :: Ord FileLevelSelection

instance isSelectionFileLevel :: IsSelection FileLevelSelection where
  toSelection AST       = ["ast"]
  toSelection LegacyAST = ["legacyAST"]

  fromSelection ["ast"]       = Just AST
  fromSelection ["legacyAST"] = Just LegacyAST
  fromSelection _             = Nothing

data EvmBytecodeOutput = BytecodeObject
                       | BytecodeOpcodes
                       | BytecodeSourceMap
                       | BytecodeLinkReferences
derive instance eqEvmBytecodeOutput  :: Eq EvmBytecodeOutput
derive instance ordEvmBytecodeOutput :: Ord EvmBytecodeOutput

instance isSelectionBytecode :: IsSelection EvmBytecodeOutput where
  toSelection BytecodeObject         = ["object"]
  toSelection BytecodeOpcodes        = ["opcodes"]
  toSelection BytecodeSourceMap      = ["sourceMap"]
  toSelection BytecodeLinkReferences = ["linkReferences"]

  fromSelection ["object"]         = Just BytecodeObject
  fromSelection ["opcodes"]        = Just BytecodeOpcodes
  fromSelection ["sourceMap"]      = Just BytecodeSourceMap
  fromSelection ["linkReferences"] = Just BytecodeLinkReferences
  fromSelection _                  = Nothing

data EvmOutputSelection = AssemblySelection
                        | LegacyAssemblySelection
                        | BytecodeSelection (Maybe EvmBytecodeOutput)
                        | DeployedBytecodeSelection (Maybe EvmBytecodeOutput)
                        | MethodIdentifiersSelection
                        | GasEstimatesSelection
derive instance eqEvmOutputSelection  :: Eq EvmOutputSelection
derive instance ordEvmOutputSelection :: Ord EvmOutputSelection

instance isSelectionEvmOutput :: IsSelection EvmOutputSelection where
  toSelection = case _ of
    AssemblySelection             -> ["assembly"]
    LegacyAssemblySelection       -> ["legacyAssembly"]
    BytecodeSelection bc          -> ["bytecode"] <> toSelection bc
    DeployedBytecodeSelection dbc -> ["deployedBytecode"] <> toSelection dbc
    MethodIdentifiersSelection    -> ["methodIdentifiers"]
    GasEstimatesSelection         -> ["gasEstimates"]

  fromSelection ["assembly"] = Just AssemblySelection
  fromSelection ["legacyAssembly"] = Just LegacyAssemblySelection
  fromSelection ["methodIdentifiers"] = Just MethodIdentifiersSelection
  fromSelection ["gasEstimates"] = Just GasEstimatesSelection
  fromSelection xs = uncons xs >>= \{head, tail} -> case head of
    "bytecode"         -> mapFromSelectionNullable BytecodeSelection tail
    "deployedBytecode" -> mapFromSelectionNullable DeployedBytecodeSelection tail
    _                  -> Nothing

data EwasmOutputSelection = Wast
                          | Wasm
derive instance eqEwasmOutputSelection  :: Eq EwasmOutputSelection
derive instance ordEwasmOutputSelection :: Ord EwasmOutputSelection

instance isSelectionEwasmOutput :: IsSelection EwasmOutputSelection where
  toSelection = case _ of
    Wast -> ["wast"]
    Wasm -> ["wasm"]

  fromSelection ["wast"] = Just Wast
  fromSelection ["wasm"] = Just Wasm
  fromSelection _        = Nothing

data ContractLevelSelection = ABI
                            | DevDoc
                            | UserDoc
                            | Metadata
                            | IR
                            | IROptimized
                            | EvmOutputSelection (Maybe EvmOutputSelection)
                            | EwasmOutputSelection (Maybe EwasmOutputSelection)
derive instance eqContractLevelSelection  :: Eq ContractLevelSelection
derive instance ordContractLevelSelection :: Ord ContractLevelSelection

instance isSelectionContractLevel :: IsSelection ContractLevelSelection where
  toSelection ABI                      = ["abi"]
  toSelection DevDoc                   = ["devdoc"]
  toSelection UserDoc                  = ["userdoc"]
  toSelection Metadata                 = ["metadata"]
  toSelection IR                       = ["ir"]
  toSelection IROptimized              = ["irOptimized"]
  toSelection (EvmOutputSelection o)   = ["evm"] <> toSelection o
  toSelection (EwasmOutputSelection o) = ["ewasm"] <> toSelection o

  fromSelection ["abi"]         = Just ABI
  fromSelection ["devdoc"]      = Just DevDoc
  fromSelection ["userdoc"]     = Just UserDoc
  fromSelection ["metadata"]    = Just Metadata
  fromSelection ["ir"]          = Just IR
  fromSelection ["irOptimized"] = Just IROptimized
  fromSelection xs = uncons xs >>= \{head, tail} -> case head of
    "evm"   -> mapFromSelectionNullable EvmOutputSelection tail
    "ewasm" -> mapFromSelectionNullable EwasmOutputSelection tail
    _       -> Nothing

newtype OutputSelection = OutputSelection
  { file     :: Array FileLevelSelection
  , contract :: ContractMapped (Array ContractLevelSelection)
  }
derive instance eqOutputSelection  :: Eq OutputSelection
derive instance ordOutputSelection :: Ord OutputSelection

instance decodeJsonOutputSelection :: DecodeJson OutputSelection where
  decodeJson j = do
    (o :: FO.Object Json) <- decodeJson j
    let Tuple fileJ contractJ = fromMaybe (Tuple jsonEmptyArray o) $ FO.pop "" o
    file <- traverse decodeJsonSelection' =<< decodeJson fileJ
    contract <- for contractJ $ (traverse decodeJsonSelection' <=< decodeJson)
    pure $ OutputSelection { file, contract }

instance encodeJsonOutputSelection :: EncodeJson OutputSelection where
  encodeJson (OutputSelection { file, contract }) =
    let fileLevelJson     = nub $ encodeJsonSelection <$> file
        contractLevelJson = (nub <<< map encodeJsonSelection) <$> contract
        allSels            = FO.insert "" fileLevelJson contractLevelJson
        nonEmptySelections = FO.filter (not <<< null) allSels
     in encodeJson nonEmptySelections

newtype OutputSelections = OutputSelections (FileMapped OutputSelection)
derive newtype instance encodeJsonOutputSelections :: EncodeJson OutputSelections
derive newtype instance eqOutputSelections :: Eq OutputSelections
derive newtype instance ordOutputSelections :: Ord OutputSelections

--------------------------------------------------
--- "settings"

newtype CompilerSettings = CompilerSettings
  { remappings      :: Array Remapping
  , optimizer       :: Maybe OptimizerSettings
  , evmVersion      :: Maybe EvmVersion
  , metadata        :: Maybe MetadataSettings
  , libraries       :: Maybe Libraries
  , outputSelection :: Maybe OutputSelections
  }

derive instance eqCompilerSettings :: Eq CompilerSettings
derive instance ordCompilerSettings :: Ord CompilerSettings

instance encodeJsonCompilerSettings :: EncodeJson CompilerSettings where
  encodeJson (CompilerSettings s) =
        "remappings"      :=? (flattenOptionalArray s.remappings)
    ~>? "optimizer"       :=? s.optimizer
    ~>? "evmVersion"      :=? s.evmVersion
    ~>? "metadata"        :=? s.metadata
    ~>? "libraries"       :=? s.libraries
    ~>? "outputSelection" :=? s.outputSelection
    ~>? jsonEmptyObject
