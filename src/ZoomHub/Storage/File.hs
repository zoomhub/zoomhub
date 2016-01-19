module ZoomHub.Storage.File
  ( getById
  , getByURL
  , create
  )
  where

import           Prelude                                  hiding (fromInteger)

import           Control.Concurrent.STM                   (TVar, atomically,
                                                           modifyTVar, readTVar)
import           Control.Exception                        (tryJust)
import           Control.Monad                            (guard)
import           Data.Aeson                               (decode)
import           Data.Aeson.Encode.Pretty                 (encodePretty')
import qualified Data.ByteString.Lazy                     as BL
import           System.AtomicWrite.Writer.LazyByteString (atomicWriteFile)
import           System.Directory                         (doesFileExist)
import           System.FilePath.Posix                    ((<.>), (</>))
import           System.IO.Error                          (isDoesNotExistError)
import           System.Posix.Files                       (createLink)

import           ZoomHub.Config                           (Config)
import qualified ZoomHub.Config                           as Config
import           ZoomHub.Storage.Internal.File            (hashURL, toFilename)
import           ZoomHub.Types.Internal.Content           (Content, contentId,
                                                           fromURL,
                                                           prettyEncodeConfig)
import           ZoomHub.Types.Internal.ContentId         (ContentId,
                                                           fromInteger, unId)


-- TODO: Introduce `ContentURL` `newtype`:
type URL = String

-- Public API
create :: Config -> String -> IO Content
create config contentURL = do
  newContentId <- createNewId config
  let newContent = fromURL newContentId contentURL
  write newContent
  writeIndex newContent contentURL
  return newContent
  where
      write :: Content -> IO ()
      write newContent = atomicWriteFile (idPath newContent) (encode newContent)

      writeIndex :: Content -> URL -> IO ()
      writeIndex content url = createLink (idPath content) (urlPath url)

      idPath content = getByIdPath (Config.dataPath config) (contentId content)
      urlPath = getByURLPath (Config.dataPath config)
      encode = encodePretty' prettyEncodeConfig

getById :: FilePath -> ContentId -> IO (Maybe Content)
getById dataPath cId = readJSON $ getByIdPath dataPath cId

getByURL :: FilePath -> URL -> IO (Maybe Content)
getByURL dataPath url = readJSON $ getByURLPath dataPath url

-- Helpers
createNewId :: Config -> IO ContentId
createNewId config = do
  newId <- incrementAndGet $ Config.lastId config
  let newContentId = fromInteger (Config.encodeId config) newId
  result <- doesIdExist (Config.dataPath config) newContentId
  if result then createNewId config else return newContentId
  where
    incrementAndGet :: TVar Integer -> IO Integer
    incrementAndGet tvar = atomically $ do
      modifyTVar tvar (+1)
      readTVar tvar

doesIdExist :: FilePath -> ContentId -> IO Bool
doesIdExist dataPath cId = doesFileExist $ getByIdPath dataPath cId

getByIdPath :: FilePath -> ContentId -> FilePath
getByIdPath dataPath cId =
  dataPath </> "content-by-id" </> filename <.> ".json"
  where filename = toFilename . unId $ cId

getByURLPath :: FilePath -> URL -> FilePath
getByURLPath dataPath url =
  dataPath </> "content-by-url" </> filename <.> ".json"
  where filename = hashURL url

readJSON :: FilePath -> IO (Maybe Content)
readJSON contentPath = do
  f <- tryJust (guard . isDoesNotExistError) (BL.readFile contentPath)
  case f of
    Left _  -> return Nothing
    Right s -> return $ decode s
