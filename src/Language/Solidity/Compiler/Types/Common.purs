module Language.Solidity.Compiler.Types.Common
  ( FileMapped(..)
  , ContractMapped(..)
  , Strung(..)
  , flattenOptionalArray
  ) where

import Prelude

import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, fromString, jsonParser, stringify)
import Data.Argonaut.Decode.Error (JsonDecodeError(..))
import Data.Bifunctor (lmap)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Foreign.Object as FO

--- Some readability sugar for nested object syntaxes
type FileMapped a     = FO.Object a
type ContractMapped a = FO.Object a

--- Some fields are arrays, and can be omitted entirely if empty
flattenOptionalArray :: forall a. Array a -> Maybe (Array a)
flattenOptionalArray rs = if Array.null rs then Nothing else Just rs

--- Some fields are strings which are really JSON
newtype Strung a = Strung a

derive newtype instance eqStrung        :: Eq a        => Eq        (Strung a)
derive newtype instance ordStrung       :: Ord a       => Ord       (Strung a)
derive newtype instance semigroupStrung :: Semigroup a => Semigroup (Strung a)
derive newtype instance monoidStrung    :: Monoid a    => Monoid    (Strung a)

derive instance newtypeStrung :: Newtype (Strung a) _

instance decodeJsonStrung :: DecodeJson a => DecodeJson (Strung a) where
  decodeJson j = 
    decodeJson j >>= 
      (lmap TypeMismatch <<< jsonParser) >>= 
      decodeJson >>= 
      pure <<< Strung

instance encodeJsonStrung :: EncodeJson a => EncodeJson (Strung a) where
  encodeJson (Strung a) = fromString <<< stringify $ encodeJson a
