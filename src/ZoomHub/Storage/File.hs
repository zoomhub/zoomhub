{-# LANGUAGE PackageImports #-}

module ZoomHub.Storage.File
  ( getById
  , getByURL
  )
  where

import           Prelude                          hiding (fromInteger)

import           Control.Concurrent.STM           (TVar, atomically, modifyTVar,
                                                   readTVar)
import           Control.Exception                (tryJust)
import           Control.Monad                    (guard)
import           Control.Monad.IO.Class           (liftIO)
import           Crypto.Hash                      (Digest, SHA256, hash)
import           Data.Aeson                       as Aeson (decode, encode)
import           Data.Aeson.Encode.Pretty         (encodePretty')
import qualified Data.ByteString.Char8            as BC
import qualified Data.ByteString.Lazy             as BL
import qualified Data.ByteString.Lazy.Char8       as BLC
import           Data.Either                      (Either (Left, Right))
import           System.IO.Error                  (isDoesNotExistError)

import qualified ZoomHub.Config                   as C
import qualified ZoomHub.Rackspace.CloudFiles     as CF
import           ZoomHub.Storage.Internal.File    (toFilename, toId)
import           ZoomHub.Types.Internal.Content   (Content, fromURL,
                                                   prettyEncodeConfig)
import           ZoomHub.Types.Internal.ContentId (ContentId, fromInteger,
                                                   fromString, unId)

-- Public API
getById :: FilePath -> ContentId -> IO (Maybe Content)
getById dataPath contentId = do
  f <- tryJust (guard . isDoesNotExistError) (BL.readFile contentPath)
  case f of
    Left _  -> return Nothing
    Right s -> return $ decode s
  where contentPath = getContentPath dataPath contentId

getByURL :: C.Config -> CF.Metadata -> String -> IO (Maybe Content)
getByURL config meta url = (getById dataPath) =<< getIdByURL config meta url
  where
    dataPath = C.dataPath config

getIdByURL :: C.Config -> CF.Metadata -> String -> IO ContentId
getIdByURL config meta url = do
  maybeContentId <- _getIdByURL meta url
  case maybeContentId of
    Nothing -> do
      newId <- incrementAndGet $ C.lastId config
      let newContentId = fromInteger encodeIntegerId newId
      let newContent = fromURL newContentId url
      BL.writeFile (getContentPath dataPath newContentId)
        (encodePretty' prettyEncodeConfig newContent)
      return newContentId
    Just contentId -> return contentId
  where
      incrementAndGet :: TVar Integer -> IO Integer
      incrementAndGet tvar = atomically $ do
        modifyTVar tvar (+1)
        readTVar tvar
      dataPath = C.dataPath config
      encodeIntegerId = C.encodeIntegerId config

_getIdByURL :: CF.Metadata -> String -> IO (Maybe ContentId)
_getIdByURL meta url = do
  cId <- CF.getContent meta urlPath
  return $ (fromString . toId . BLC.unpack) <$> cId
  where
    sha256 x = show (hash $ BC.pack x :: Digest SHA256)
    urlPath = "/content/content-by-url/" ++ (sha256 url) ++ ".txt"

-- Helpers
getContentPath :: FilePath -> ContentId -> String
getContentPath dataPath contentId =
  dataPath ++ "/content-by-id/" ++ filename ++ ".json"
  where filename = toFilename . unId $ contentId
