{-# LANGUAGE PackageImports #-}

module ZoomHub.Storage.File
  ( store
  , load
  -- TEMP: Expose private functions during refactoring:
  , getContentPath
  , getContentIdFromURL
  )
  where

import "cryptonite" Crypto.Hash (hash, Digest, SHA256)
import Control.Exception (tryJust)
import Control.Monad (guard)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson as Aeson (encode, decode)
import Data.Either (Either(..))
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified ZoomHub.Rackspace.CloudFiles as CF
import System.IO.Error (isDoesNotExistError)
import ZoomHub.Storage.Internal.File (toFilename, toId)
import ZoomHub.Types.Internal.Content (Content)
import ZoomHub.Types.Internal.ContentId (ContentId, fromLBS, unId)

-- Public API
store :: Content -> IO ()
store = undefined

load :: FilePath -> ContentId -> IO (Maybe Content)
load dataPath contentId = do
  f <- tryJust (guard . isDoesNotExistError) (BL.readFile contentPath)
  case f of
    Left _  -> return Nothing
    Right s -> return $ decode s
  where contentPath = getContentPath dataPath contentId

-- Helpers
getContentPath :: FilePath -> ContentId -> String
getContentPath dataPath contentId =
  dataPath ++ "/content-by-id/" ++ filename ++ ".json"
  where filename = toFilename . unId $ contentId

getContentIdFromURL :: CF.Metadata -> String -> IO (Maybe ContentId)
getContentIdFromURL meta url = do
  cId <- CF.getContent meta urlPath
  return $ fromLBS <$> (BLC.pack . toId . BLC.unpack) <$> cId
  where
    sha256 x = show (hash $ BC.pack x :: Digest SHA256)
    urlPath = "/content/content-by-url/" ++ (sha256 url) ++ ".txt"
