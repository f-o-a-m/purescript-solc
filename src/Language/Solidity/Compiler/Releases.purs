module Language.Solidity.Compiler.Releases
  ( Build(..)
  , ReleaseList(..)
  , BuildR
  , ReleaseRepo
  , defaultReleaseRepo
  , getReleaseList
  , getReleaseSource
  , getURL
  ) where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Except (ExceptT(..), catchError, except, runExceptT)
import Data.Argonaut (class DecodeJson, class EncodeJson, decodeJson, encodeJson, jsonParser)
import Data.Argonaut.Decode.Error (printJsonDecodeError)
import Data.Bifunctor (lmap)
import Data.Either (Either(..), note)
import Data.Maybe (Maybe(..))
import Data.String (toLower)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Foreign.Object as FO

type BuildR a = Record
  ( path :: String
  , version :: String
  , build :: String
  , longVersion :: String
  , keccak256 :: String
  , urls :: Array String
  | a
  )

data Build
  = Stable (BuildR ())
  | Prerelease (BuildR (prerelease :: String))

instance decodeJsonBuild :: DecodeJson Build where
  decodeJson j = Prerelease <$> decodeJson j <|> Stable <$> decodeJson j

instance encodeJsonBuild :: EncodeJson Build where
  encodeJson (Stable s) = encodeJson s
  encodeJson (Prerelease s) = encodeJson s

newtype ReleaseList =
  ReleaseList
    { builds :: Array Build
    , releases :: FO.Object String
    , latestRelease :: String
    }

derive newtype instance decodeJsonReleaseList :: DecodeJson ReleaseList
derive newtype instance encodeJsonReleaseList :: EncodeJson ReleaseList

newtype ReleaseRepo =
  ReleaseRepo
    { base :: String
    , listFile :: String
    }

foreign import _getURL :: String -> EffectFnAff String

getURL
  :: forall m
   . MonadAff m
  => String
  -> m (Either String String)
getURL u = liftAff $ (map Right <<< fromEffectFnAff $ _getURL u) `catchError` (pure <<< Left <<< show)

defaultReleaseRepo :: ReleaseRepo
defaultReleaseRepo = ReleaseRepo
  { base: "https://ethereum.github.io/solc-bin/bin"
  , listFile: "list.json"
  }

getReleaseList
  :: forall m
   . MonadAff m
  => ReleaseRepo
  -> m (Either String ReleaseList)
getReleaseList (ReleaseRepo repo) = runExceptT do
  let repoList = repo.base <> "/" <> repo.listFile
  rawJson <- ExceptT $ getURL repoList
  except $ jsonParser rawJson >>= (lmap printJsonDecodeError <<< decodeJson)

lookupLatestRelease
  :: ReleaseList
  -> Either String String
lookupLatestRelease (ReleaseList list) =
  note "repo's latest release was not in the repo's releases list" $
    FO.lookup list.latestRelease list.releases

getReleaseSource
  :: forall m
   . MonadAff m
  => ReleaseRepo
  -> String
  -> m (Either String String)
getReleaseSource rr@(ReleaseRepo repo) release = runExceptT do
  let fetch u = ExceptT <<< getURL $ repo.base <> "/" <> u
  rl@(ReleaseList list) <- ExceptT $ getReleaseList rr
  case toLower release of
    "latest" -> do
      releaseFileName <- except (lookupLatestRelease rl)
      fetch releaseFileName
    _ -> case FO.lookup release list.releases of
      Nothing -> fetch release
      Just releaseFilename -> fetch releaseFilename

