{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Pipeline
  ( process
  )
  where

import           Codec.MIME.Parse                         (parseMIMEType)
import           Control.Lens                             ((^.))
import           Data.Aeson                               ((.=))
import           Network.Wreq                             (get, responseBody)
import           System.AtomicWrite.Writer.LazyByteString (atomicWriteFile)
import           System.Directory                         (createDirectoryIfMissing)
import           System.FilePath.Posix                    ((<.>), (</>))
-- import           System.IO.Temp                           (withTempDirectory)
import           System.Posix                             (fileSize,
                                                           getFileStatus)
import           System.Process                           (callProcess)

import           ZoomHub.Config                           (Config)
import qualified ZoomHub.Config                           as Config
import           ZoomHub.Log.Logger                       (logInfo)
import           ZoomHub.Storage.SQLite                   (markAsActive,
                                                           markAsSuccess)
import           ZoomHub.Types.Content                    (Content, contentId,
                                                           contentURL)
import           ZoomHub.Types.ContentId                  (unId)
import           ZoomHub.Types.ContentMIME                (ContentMIME (ContentMIME))
import           ZoomHub.Types.ContentURI                 (ContentURI)
import           ZoomHub.Types.DeepZoomImage              (DeepZoomImage,
                                                           TileFormat (JPEG), TileOverlap (TileOverlap1), TileSize (TileSize254),
                                                           dziHeight,
                                                           dziTileFormat,
                                                           dziTileOverlap,
                                                           dziTileSize,
                                                           dziWidth,
                                                           mkDeepZoomImage)

process :: Config -> Content -> IO Content
process config content = do
  -- withTempDirectory tempPath template $ \tmpDir -> do
    -- let rawPath = tmpDir </> rawContentId
    let rawPath = tempPath </> rawContentId
        dziPath = rawPath <.> ".dzi"

    createDirectoryIfMissing False tempPath

    logInfo "Mark content as active" ["id" .= contentId content]
    activeContent <- markAsActive (Config.dbConnection config) content

    logInfo "Download content"
      [ "id" .= contentId content
      , "url" .= contentURL content
      ]
    downloadURL (contentURL content) rawPath

    logInfo "Create DZI" ["id" .= contentId content]
    createDZI rawPath dziPath
    -- TODO: Implement DZI parsing from output file:
    let dzi = mkDeepZoomImage 1024 1024 TileSize254 TileOverlap1 JPEG
        maybeMime = ContentMIME <$> parseMIMEType "image/jpeg"
    rawSize <- getFileSize rawPath
    logInfo "Mark content as successfully completed" ["id" .= contentId content]
    markAsSuccess conn activeContent dzi maybeMime rawSize
  where
    conn = Config.dbConnection config
    tempPath = Config.dataPath config </> "content-raw"
    rawContentId = unId $ contentId content
    -- template = rawContentId ++ "."

    getFileSize :: FilePath -> IO Integer
    getFileSize path = (toInteger . fileSize) <$> getFileStatus path

downloadURL :: ContentURI -> FilePath -> IO ()
downloadURL url dest = do
  resp <- get (show url)
  let body = resp ^. responseBody
  atomicWriteFile dest body

-- createDZI ::  FilePath -> FilePath -> IO DeepZoomImage
createDZI :: FilePath -> FilePath -> IO ()
createDZI src dest =
  callProcess "vips"
    [ "dzsave"
    , "--tile-size", show TileSize254
    , "--overlap", show TileOverlap1
    , src
    , dest
    , "--vips-progress"
    ]
