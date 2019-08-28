module Language.Solidity.Compiler.Types.Input 
  ( SourceLanguage(..)
  , Source(..)
  , Sources(..)
  , CompilerInput(..)
  , module Language.Solidity.Compiler.Types.Settings
  ) where

import Prelude

import Data.Argonaut (class EncodeJson, (:=), (:=?), (~>), (~>?), encodeJson, jsonEmptyObject)
import Data.Argonaut as A
import Data.Maybe (Maybe(..))
import Foreign.Object as FO
import Network.Ethereum.Types (HexString)
import Language.Solidity.Compiler.Types.Settings (CompilerSettings(..), Remapping(..))
--------------------------------------------------
--- "language" field of input
data SourceLanguage = Solidity | Yul

instance showSourceLanguage :: Show SourceLanguage where
  show Solidity = "Solidity"
  show Yul      = "Yul"

instance encodeJsonSourceLanguage :: EncodeJson SourceLanguage where
  encodeJson = A.fromString <<< show

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

instance encodeJsonSource :: EncodeJson Source where
  encodeJson (FromURLs u) =
       "urls"      :=  u.urls
    ~> "keccak256" :=? u.keccak256
  encodeJson (FromContent c) =
       "content"   :=  c.content
    ~> "keccak256" :=? c.keccak256

newtype Sources = Sources (FO.Object Source)

derive newtype instance encodeJsonSources :: EncodeJson Sources 

--------------------------------------------------
--- the input object itself

newtype CompilerInput = CompilerInput
  { language :: SourceLanguage
  , sources  :: Sources
  , settings :: Maybe CompilerSettings
  }

instance encodeJsonCompilerInput :: EncodeJson CompilerInput where
  encodeJson (CompilerInput i) =
       "language" :=  i.language
    ~> "sources"  :=  i.sources
    ~> "settings" :=? i.settings
    ~>? jsonEmptyObject