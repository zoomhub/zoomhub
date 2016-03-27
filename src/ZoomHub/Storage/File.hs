module ZoomHub.Storage.File
  ( getById
  , getByURL
  )
  where

import           Prelude                                  hiding (fromInteger)

import           Control.Exception                        (tryJust)
import           Control.Monad                            (guard)
import           Data.Aeson                               (decode)
import qualified Data.ByteString.Lazy                     as BL
import           System.FilePath.Posix                    ((<.>), (</>))
import           System.IO.Error                          (isDoesNotExistError)

import           ZoomHub.Storage.Internal.File            (hashURL, toFilename)
import           ZoomHub.Types.Internal.Content           (Content)
import           ZoomHub.Types.Internal.ContentId         (ContentId, unId)


-- TODO: Introduce `ContentURL` `newtype`:
type URL = String

-- Public API
getById :: FilePath -> ContentId -> IO (Maybe Content)
getById dataPath cId = readJSON $ getByIdPath dataPath cId

getByURL :: FilePath -> URL -> IO (Maybe Content)
getByURL dataPath url = readJSON $ getByURLPath dataPath url

-- Helpers
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
