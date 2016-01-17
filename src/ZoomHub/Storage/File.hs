{-# LANGUAGE PackageImports #-}

module ZoomHub.Storage.File
  ( getById
  , getByURL
  , create
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
import           ZoomHub.Types.Internal.Content   (Content, contentId, fromURL,
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
getByURL config meta url = do
  maybeContentId <- getIdByURL meta url
  case maybeContentId of
    Nothing        -> return Nothing
    Just contentId -> getById (C.dataPath config) contentId

create :: C.Config -> String -> IO Content
create config url = do
  newId <- incrementAndGet $ C.lastId config
  let newContentId = fromInteger (C.encodeId config) newId
  let newContent = fromURL newContentId url
  write newContent
  return newContent
  where
      incrementAndGet :: TVar Integer -> IO Integer
      incrementAndGet tvar = atomically $ do
        modifyTVar tvar (+1)
        readTVar tvar
      path content = getContentPath (C.dataPath config) (contentId content)
      encode = encodePretty' prettyEncodeConfig
      write newContent =
        BL.writeFile (path newContent) (encode newContent)

-- Helpers
getIdByURL :: CF.Metadata -> String -> IO (Maybe ContentId)
getIdByURL meta url = do
  cId <- CF.getContent meta urlPath
  return $ (fromString . toId . BLC.unpack) <$> cId
  where
    sha256 x = show (hash $ BC.pack x :: Digest SHA256)
    urlPath = "/content/content-by-url/" ++ (sha256 url) ++ ".txt"

getContentPath :: FilePath -> ContentId -> String
getContentPath dataPath contentId =
  dataPath ++ "/content-by-id/" ++ filename ++ ".json"
  where filename = toFilename . unId $ contentId
