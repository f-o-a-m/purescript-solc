module Language.Solidity.Compiler.Types.Input 
  ( SourceLanguage(..)
  , Source(..)
  , Sources(..)
  , CompilerInput(..)
  , module Language.Solidity.Compiler.Types.Settings
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, (:=), (:=?), (~>), (~>?), decodeJson, jsonEmptyObject)
import Data.Argonaut as A
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Newtype (class Newtype)
import Foreign.Object as FO
import Network.Ethereum.Types (HexString)
import Language.Solidity.Compiler.Types.Settings (class IsSelection, CompilerSettings(..), ContractLevelSelection(..), EvmBytecodeOutput(..), EvmOutputSelection(..), EvmVersion(..), EwasmOutputSelection(..), FileLevelSelection(..), Libraries(..), Library(..), MetadataSettings(..), OptimizerDetails(..), OptimizerSettings(..), OutputSelection(..), OutputSelections(..), Remapping(..), YulOptimizerDetails(..), decodeJsonSelection, encodeJsonSelection, fromSelection, toSelection)

--------------------------------------------------
--- "language" field of input
data SourceLanguage = Solidity | Yul
derive instance eqSourceLanguage :: Eq SourceLanguage
derive instance ordSourceLanguage :: Ord SourceLanguage

instance decodeJsonSourceLanguage :: DecodeJson SourceLanguage where
  decodeJson j = decodeJson j >>= case _ of
    "Solidity" -> pure Solidity
    "Yul"      -> pure Yul
    x          -> Left $ Named ("Unknown source language " <> x) $ UnexpectedValue j

instance encodeJsonSourceLanguage :: EncodeJson SourceLanguage where
  encodeJson = A.fromString <<< case _ of
    Solidity -> "Solidity"
    Yul      -> "Yul"

--------------------------------------------------
--- "sources" field of input

data Source = 
    FromURLs 
      { keccak256 :: Maybe HexString -- todo: enforce 256 bit size?
      , urls      :: Array String
      }
  | FromContent
      { keccak256 :: Maybe HexString -- todo: enforce 256 bit size?
      , content   :: String
      }
derive instance eqSource  :: Eq Source
derive instance ordSource :: Ord Source

instance encodeJsonSource :: EncodeJson Source where
  encodeJson (FromURLs u) =
       "urls"      :=  u.urls
    ~> "keccak256" :=? u.keccak256
  encodeJson (FromContent c) =
       "content"   :=  c.content
    ~> "keccak256" :=? c.keccak256

newtype Sources = Sources (FO.Object Source)
derive instance newtypeSources :: Newtype Sources _
derive newtype instance encodeJsonSources :: EncodeJson Sources
derive newtype instance eqSources :: Eq Sources
derive newtype instance ordSources :: Ord Sources

--------------------------------------------------
--- the input object itself

newtype CompilerInput = CompilerInput
  { language :: SourceLanguage
  , sources  :: Sources
  , settings :: Maybe CompilerSettings
  }
derive instance eqCompilerInput  :: Eq CompilerInput
derive instance ordCompilerInput :: Ord CompilerInput

instance encodeJsonCompilerInput :: EncodeJson CompilerInput where
  encodeJson (CompilerInput i) =
       "language" :=  i.language
    ~> "sources"  :=  i.sources
    ~> "settings" :=? i.settings
    ~>? jsonEmptyObject
