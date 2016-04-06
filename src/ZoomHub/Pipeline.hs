{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Pipeline
  ( process
  )
  where

import           Codec.MIME.Parse                         (parseMIMEType)
import qualified Codec.MIME.Type                          as MIME
import           Control.Lens                             ((^.))
import           Data.Aeson                               ((.=))
import           Data.Monoid                              ((<>))
import           Data.Text.Encoding                       (decodeUtf8)
import           Network.Wreq                             (get, responseBody,
                                                           responseHeader)
import           System.AtomicWrite.Writer.LazyByteString (atomicWriteFile)
import           System.Directory                         (createDirectoryIfMissing,
                                                           listDirectory)
import           System.FilePath.Posix                    (dropExtension, (<.>),
                                                           (</>))
-- import           System.IO.Temp                           (withTempDirectory)
import           System.Posix                             (fileSize,
                                                           getFileStatus)
import           System.Process                           (callProcess)

import           ZoomHub.Config                           (Config)
import qualified ZoomHub.Config                           as Config
import           ZoomHub.Log.Logger                       (logInfo)
import           ZoomHub.Storage.SQLite                   (markAsActive,
                                                           markAsFailure,
                                                           markAsSuccess)
import           ZoomHub.Types.Content                    (Content, contentId,
                                                           contentURL)
import           ZoomHub.Types.ContentId                  (unId)
import           ZoomHub.Types.ContentMIME                (ContentMIME (ContentMIME))
import           ZoomHub.Types.ContentURI                 (ContentURI)
import           ZoomHub.Types.DeepZoomImage              (DeepZoomImage, TileFormat (JPEG, PNG), TileOverlap (TileOverlap1), TileSize (TileSize254),
                                                           fromXML)

process :: Config -> Content -> IO Content
process config content = do
    createDirectoryIfMissing True tempPath

  -- withTempDirectory tempPath template $ \tmpDir -> do
    let rawPath = tempPath </> rawContentId
    -- let rawPath = tmpDir </> rawContentId
        dziPath = rawPath <.> ".dzi"

    logInfo "Create temporary working directory"
      ["id" .= contentId content, "path" .= tempPath]
    createDirectoryIfMissing True tempPath

    logInfo "Mark content as active" ["id" .= contentId content]
    activeContent <- markAsActive conn content

    logInfo "Download content"
      [ "id" .= contentId content
      , "url" .= contentURL content
      ]
    maybeMIME <-
      ((<$>) . (<$>)) ContentMIME $ downloadURL (contentURL content) rawPath
    rawSize <- getFileSize rawPath

    logInfo "Create DZI"
      ["id" .= contentId content]
    maybeDZI <- createDZI rawPath dziPath (toTileFormat maybeMIME)

    logInfo "Content metadata"
      [ "mime" .= maybeMIME
      , "size" .= rawSize
      , "dzi" .= maybeDZI
      ]

    case maybeDZI of
      Just dzi -> do
        logInfo "Upload DZI"
          [ "id" .= contentId content
          , "dzi" .= dzi
          , "dziPath" .= dziPath
          ]
        uploadDZI dziPath

        logInfo "Succeeded to process content"
          ["id" .= contentId content]
        markAsSuccess conn activeContent dzi maybeMIME rawSize

      _ -> do
        logInfo "Failed to process content"
          ["id" .= contentId content]
        let error_ = Nothing
        markAsFailure conn activeContent error_
  where
    conn = Config.dbConnection config
    tempPath = Config.dataPath config </> "content-raw"
    rawContentId = unId $ contentId content
    -- template = rawContentId ++ "_"

    getFileSize :: FilePath -> IO Integer
    getFileSize path = (toInteger . fileSize) <$> getFileStatus path

downloadURL :: ContentURI -> FilePath -> IO (Maybe MIME.Type)
downloadURL url dest = do
  res <- get (show url)
  let body = res ^. responseBody
  atomicWriteFile dest body
  return $ parseMIMEType . decodeUtf8 $ res ^. responseHeader "content-type"

createDZI :: FilePath -> FilePath -> TileFormat -> IO (Maybe DeepZoomImage)
createDZI src dest tileFormat = do
  callProcess "vips"
    [ "dzsave"
    , "--tile-size=" <> show TileSize254
    , "--overlap=" <> show TileOverlap1
    , src
    , dest
    , "--suffix=" <> toVIPSSuffix tileFormat
    , "--vips-progress"
    ]
  dzi <- readFile dest
  return $ fromXML dzi

uploadDZI :: FilePath -> IO ()
uploadDZI dziPath = do
  tilePaths <- getDZITilePaths dziPath
  print tilePaths

getDZITilePaths :: FilePath -> IO [FilePath]
getDZITilePaths dziPath = do
    levelDirs <- map (filesDir </>) <$> listDirectory filesDir
    concat <$> mapM (\levelDir -> map (levelDir </>) <$> listDirectory levelDir)
      levelDirs
  where
    filesDir = dropExtension dziPath <> "_files"

toTileFormat :: Maybe ContentMIME -> TileFormat
toTileFormat (Just (ContentMIME (MIME.Type (MIME.Image "png") _))) = PNG
toTileFormat _                                                     = JPEG

toVIPSSuffix :: TileFormat -> String
toVIPSSuffix PNG = ".png"
toVIPSSuffix JPEG = ".jpg[Q=90]"
