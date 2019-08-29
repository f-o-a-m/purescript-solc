module Language.Solidity.Compiler.Types.Settings 
  ( Remapping(..)
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
  ) where

import Prelude

import Data.Argonaut (class EncodeJson, encodeJson, jsonEmptyObject, jsonSingletonObject, (:=?), (~>?))
import Data.Argonaut as A
import Data.Array (nub, null)
import Data.Maybe (Maybe(..), maybe)
import Foreign.Object as FO
import Network.Ethereum.Types (Address)
import Node.Path (FilePath)

import Language.Solidity.Compiler.Types.Common (ContractMapped, FileMapped, flattenOptionalArray)

--------------------------------------------------
--- "remappings" field of "settings" field
--- NB: this is a single remapping

data Remapping = GlobalRemapping { to :: FilePath}
               | Remapping { from :: FilePath, to :: FilePath }

instance encodeJsonRemapping :: EncodeJson Remapping where
  encodeJson = A.fromString <<< case _ of
    GlobalRemapping g -> ":g=" <> g.to
    Remapping r       -> r.from <> "=" <> r.to

--------------------------------------------------
--- "settings.optimizer.yulDetails"

newtype YulOptimizerDetails = YulOptimizerDetails
  { stackAllocation :: Boolean
  }

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

instance encodeJsonOptimizerSettings :: EncodeJson OptimizerSettings where
  encodeJson (OptimizerSettings o) =
        "enabled" :=? o.enabled
    ~>? "runs"    :=? o.runs
    ~>? "details" :=? o.details
    ~>? jsonEmptyObject

--------------------------------------------------
--- "settings.evmVersion"

data EvmVersion = Homestead
                | TangerineWhistle
                | SpuriousDragon
                | Byzantium
                | Constantinople
                | Petersburg

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

instance encodeJsonMetadataSettings :: EncodeJson MetadataSettings where
  encodeJson (MetadataSettings ms) = 
    jsonSingletonObject "useLiteralContent" $ A.fromBoolean ms.useLiteralContent

--------------------------------------------------
--- "settings.libraries"

newtype Library = Library
  { libraryName :: String
  , address     :: Address
  }

instance encodeJsonLibrary :: EncodeJson Library where
  encodeJson (Library l) =
    jsonSingletonObject l.libraryName (encodeJson l.address)

newtype Libraries = Libraries (FileMapped Library)
derive newtype instance encodeJsonLibraries :: EncodeJson Libraries 

--------------------------------------------------
--- "settings.outputSelection"

class ToSelection a where
  toSelection :: a -> String 

data FileLevelSelection = AST
                        | LegacyAST

instance toSelectionFileLevel :: ToSelection FileLevelSelection where
  toSelection AST       = "ast"
  toSelection LegacyAST = "legacyAST"

data EvmBytecodeOutput = BytecodeObject
                       | BytecodeOpcodes
                       | BytecodeSourceMap
                       | BytecodeLinkReferences

instance toSelectionBytecode :: ToSelection EvmBytecodeOutput where
  toSelection BytecodeObject         = "object"
  toSelection BytecodeOpcodes        = "opcodes"
  toSelection BytecodeSourceMap      = "sourceMap"
  toSelection BytecodeLinkReferences = "linkReferences"

data EvmOutputSelection = AssemblySelection
                        | LegacyAssemblySelection
                        | BytecodeSelection (Maybe EvmBytecodeOutput)
                        | DeployedBytecodeSelection (Maybe EvmBytecodeOutput)
                        | MethodIdentifiersSelection
                        | GasEstimatesSelection

instance toSelectionEvmOutput :: ToSelection (Maybe EvmOutputSelection) where
  toSelection Nothing  = "evm"
  toSelection (Just o) = "evm." <> case o of
    AssemblySelection             -> "assembly"
    LegacyAssemblySelection       -> "legacyAssembly"
    BytecodeSelection bc          -> 
      case bc of 
        Nothing  -> "bytecode"
        Just bc' -> "bytecode." <> toSelection bc'
    DeployedBytecodeSelection dbc -> 
      case dbc of
        Nothing   -> "deployedBytecode"
        Just dbc' -> "deployedBytecode." <> toSelection dbc'
    MethodIdentifiersSelection    -> "methodIdentifiers"
    GasEstimatesSelection         -> "gasEstimates"

data EwasmOutputSelection = Wast
                          | Wasm

instance toSelectionEwasmOutput :: ToSelection (Maybe EwasmOutputSelection) where
  toSelection Nothing  = "ewasm"
  toSelection (Just o) = "ewasm." <> case o of
    Wast -> "wast"
    Wasm -> "wasm"

data ContractLevelSelection = ABI
                            | DevDoc
                            | UserDoc
                            | Metadata
                            | IR
                            | IROptimized
                            | EvmOutputSelection (Maybe EvmOutputSelection)
                            | EwasmOutputSelection (Maybe EwasmOutputSelection)

instance toSelectionContractLevel :: ToSelection ContractLevelSelection where
  toSelection ABI                      = "abi"
  toSelection DevDoc                   = "devdoc"
  toSelection UserDoc                  = "userdoc"
  toSelection Metadata                 = "metadata"
  toSelection IR                       = "ir"
  toSelection IROptimized              = "irOptimized"
  toSelection (EvmOutputSelection o)   = toSelection o
  toSelection (EwasmOutputSelection o) = toSelection o

newtype OutputSelection = OutputSelection
  { file     :: Array FileLevelSelection
  , contract :: ContractMapped (Array ContractLevelSelection)
  }

instance encodeJsonOutputSelection :: EncodeJson OutputSelection where
  encodeJson (OutputSelection { file, contract }) = encodeJson $
    let fileSels             = flattenOptionalArray $ nub $ toSelection <$> file
        contractSels         = (nub <<< map toSelection) <$> contract
        filteredContractSels = FO.filter null contractSels
     in maybe filteredContractSels (flip (FO.insert "") filteredContractSels) fileSels

newtype OutputSelections = OutputSelections (FileMapped OutputSelection)
derive newtype instance encodeJsonOutputSelections :: EncodeJson OutputSelections

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

instance encodeJsonCompilerSettings :: EncodeJson CompilerSettings where
  encodeJson (CompilerSettings s) =
        "remappings"      :=? (flattenOptionalArray s.remappings)
    ~>? "optimizer"       :=? s.optimizer
    ~>? "evmVersion"      :=? s.evmVersion
    ~>? "metadata"        :=? s.metadata
    ~>? "libraries"       :=? s.libraries
    ~>? "outputSelection" :=? s.outputSelection
    ~>? jsonEmptyObject
