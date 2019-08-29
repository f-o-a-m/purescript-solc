module Language.Solidity.Compiler.Types.Common
  ( FileMapped(..)
  , ContractMapped(..)
  , Strung(..)
  , flattenOptionalArray
  ) where

import Prelude
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, fromString, jsonParser, stringify)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, wrap, unwrap)
import Foreign.Object as FO

--- Some readability sugar for nested object syntaxes
type FileMapped a     = FO.Object a
type ContractMapped a = FO.Object a

--- Some fields are arrays, and can be omitted entirely if empty
flattenOptionalArray :: forall a. Array a -> Maybe (Array a)
flattenOptionalArray rs = if Array.null rs then Nothing else Just rs

--- Some fields are strings which are really JSON
newtype Strung a = Strung a

instance newtypeStrung :: Newtype (Strung a) a where
  wrap = Strung
  unwrap (Strung a) = a

instance decodeJsonStrung :: DecodeJson a => DecodeJson (Strung a) where
  decodeJson j = decodeJson j >>= jsonParser >>= decodeJson >>= pure <<< wrap

instance encodeJsonStrung :: EncodeJson a => EncodeJson (Strung a) where
  encodeJson = fromString <<< stringify <<< encodeJson <<< unwrap