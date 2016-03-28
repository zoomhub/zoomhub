{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Pipeline
  ( process
  )
  where

import           Control.Lens                             ((^.))
import           Data.Aeson                               ((.=))
import           Network.Wreq                             (get, responseBody)
import           System.AtomicWrite.Writer.LazyByteString (atomicWriteFile)
import           System.Directory                         (createDirectoryIfMissing)
import           System.FilePath.Posix                    ((<.>), (</>))
-- import           System.IO.Temp
import           System.Process                           (callProcess)

import           ZoomHub.Config                           (Config)
import qualified ZoomHub.Config                           as Config
import           ZoomHub.Log.Logger                       (logInfo)
import           ZoomHub.Storage.SQLite                   (markAsActive,
                                                           markAsSuccess)
import           ZoomHub.Types.Content                    (Content, contentId,
                                                           contentUrl)
import           ZoomHub.Types.ContentId                  (unId)
import           ZoomHub.Types.ContentURI                 (ContentURI)
import           ZoomHub.Types.DeepZoomImage              (TileOverlap (TileOverlap1), TileSize (TileSize254))

process :: Config -> Content -> IO Content
process config content = do
  -- withTempDirectory tempPath template $ \tmpDir -> do
    -- let rawPath = tmpDir </> rawContentId
    let rawPath = tempPath </> rawContentId
    createDirectoryIfMissing False tempPath

    logInfo "Mark content as active" ["id" .= contentId content]
    activeContent <- markAsActive (Config.dbConnection config) content

    logInfo "Download content"
      [ "id" .= contentId content
      , "url" .= contentUrl content
      ]
    downloadURL (contentUrl content) rawPath

    logInfo "Create DZI" ["contentId" .= contentId content]
    createDZI rawPath (rawPath <.> ".dzi")

    logInfo "Set content state: 'completed:success'"
      ["contentId" .= contentId content]
    markAsSuccess (Config.dbConnection config) activeContent
  where
    tempPath = Config.dataPath config </> "content-raw"
    rawContentId = unId $ contentId content
    -- template = rawContentId ++ "."

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
