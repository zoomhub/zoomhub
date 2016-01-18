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
import           Data.Aeson                       as Aeson (decode, encode)
import           Data.Aeson.Encode.Pretty         (encodePretty')
import qualified Data.ByteString.Lazy             as BL
import qualified Data.ByteString.Lazy.Char8       as BLC
import           Data.Either                      (Either (Left, Right))
import           System.FilePath.Posix            ((<.>), (</>))
import           System.IO.Error                  (isDoesNotExistError)

import qualified ZoomHub.Config                   as C
import           ZoomHub.Storage.Internal.File    (hashURL, toFilename, toId)
import           ZoomHub.Types.Internal.Content   (Content, contentId, fromURL,
                                                   prettyEncodeConfig)
import           ZoomHub.Types.Internal.ContentId (ContentId, fromInteger,
                                                   fromString, unId)

-- Public API
type URL = String

getById :: FilePath -> ContentId -> IO (Maybe Content)
getById dataPath contentId = readJSON $ getByIdPath dataPath contentId

getByURL :: FilePath -> URL -> IO (Maybe Content)
getByURL dataPath url = readJSON $ getByURLPath dataPath url

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
      path content = getByIdPath (C.dataPath config) (contentId content)
      encode = encodePretty' prettyEncodeConfig
      write newContent =
        BL.writeFile (path newContent) (encode newContent)

-- Helpers
getByURLPath :: FilePath -> URL -> FilePath
getByURLPath dataPath url =
  dataPath </> "content-by-url" </> filename <.> ".json"
  where filename = hashURL url

getByIdPath :: FilePath -> ContentId -> FilePath
getByIdPath dataPath contentId =
  dataPath </> "content-by-id" </> filename <.> ".json"
  where filename = toFilename . unId $ contentId

readJSON :: FilePath -> IO (Maybe Content)
readJSON contentPath = do
  f <- tryJust (guard . isDoesNotExistError) (BL.readFile contentPath)
  case f of
    Left _  -> return Nothing
    Right s -> return $ decode s
