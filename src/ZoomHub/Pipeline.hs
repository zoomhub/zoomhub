{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Pipeline
  ( process
  )
  where

import           Codec.MIME.Parse                         (parseMIMEType)
import qualified Codec.MIME.Type                          as MIME
import           Control.Concurrent.Async                 (forConcurrently)
import           Control.Lens                             ((^.))
import           Control.Monad                            (when)
import           Data.Aeson                               ((.=))
import           Data.List                                (stripPrefix)
import           Data.List.Split                          (chunksOf)
import           Data.Monoid                              ((<>))
import           Data.Text.Encoding                       (decodeUtf8)
import           Network.Wreq                             (get, responseBody,
                                                           responseHeader)
import           System.AtomicWrite.Writer.LazyByteString (atomicWriteFile)
import           System.Directory                         (listDirectory)
import           System.Exit                              (ExitCode (ExitSuccess))
import           System.FilePath                          (addTrailingPathSeparator,
                                                           dropExtension, (<.>),
                                                           (</>))
import           System.IO.Temp                           (withTempDirectory)
import           System.Posix                             (fileSize,
                                                           getFileStatus)
import           System.Process                           (readProcessWithExitCode)

import           ZoomHub.Config                           (RackspaceConfig,
                                                           raxApiKey,
                                                           raxContainer,
                                                           raxContainerPath,
                                                           raxUsername)
import           ZoomHub.Log.Logger                       (logDebugT, logError,
                                                           logInfo, logInfoT)
import           ZoomHub.Rackspace.CloudFiles             (ObjectName,
                                                           getMetadata,
                                                           mkCredentials,
                                                           parseObjectName,
                                                           putContent)
import           ZoomHub.Types.Content                    (Content, contentId,
                                                           contentURL, contentSize, contentMIME, contentDZI)
import           ZoomHub.Types.ContentId                  (unId)
import           ZoomHub.Types.ContentMIME                (ContentMIME (ContentMIME))
import           ZoomHub.Types.ContentURI                 (ContentURI)
import           ZoomHub.Types.DeepZoomImage              (DeepZoomImage, TileFormat (JPEG, PNG), TileOverlap (TileOverlap1), TileSize (TileSize254),
                                                           dziTileFormat,
                                                           fromXML)
import           ZoomHub.Types.TempPath                   (TempPath, unTempPath)


process :: String -> RackspaceConfig -> TempPath -> Content -> IO Content
process workerId raxConfig tempPath content =
  withTempDirectory (unTempPath tempPath) template $ \tmpDir -> do
    let rawPathPrefix = tmpDir </> rawContentId
        rawPath = rawPathPrefix ++ ".raw"
        dziPath = rawPathPrefix <.> "dzi"

    logInfo "Create temporary working directory"
      [ "id" .= contentId content
      , "path" .= tmpDir
      , "worker" .= workerId
      ]

    maybeMIME <- logInfoT "Download content"
      [ "id" .= contentId content
      , "url" .= contentURL content
      , "worker" .= workerId
      ] $ ((<$>) . (<$>)) ContentMIME $
          downloadURL (contentURL content) rawPath

    rawSize <- getFileSize rawPath

    dzi <- logInfoT "Create DZI"
      [ "id" .= contentId content
      , "worker" .= workerId
      ] $ createDZI workerId rawPath dziPath (toTileFormat maybeMIME)

    logInfo "Content metadata"
      [ "mime" .= maybeMIME
      , "size" .= rawSize
      , "dzi" .= dzi
      , "worker" .= workerId
      ]

    logInfoT "Upload DZI"
      [ "id" .= contentId content
      , "dzi" .= dzi
      , "dziPath" .= dziPath
      , "worker" .= workerId
      ] $ uploadDZI workerId raxConfig tmpDir dziPath dzi

    return content
      { contentMIME = maybeMIME
      , contentSize = Just rawSize
      , contentDZI = Just dzi
      }
  where
    rawContentId = unId (contentId content)
    template = rawContentId ++ "-"

    getFileSize :: FilePath -> IO Integer
    getFileSize path = (toInteger . fileSize) <$> getFileStatus path

downloadURL :: ContentURI -> FilePath -> IO (Maybe MIME.Type)
downloadURL url dest = do
  res <- get (show url)
  let body = res ^. responseBody
  atomicWriteFile dest body
  return $ parseMIMEType . decodeUtf8 $ res ^. responseHeader "content-type"

createDZI :: String -> FilePath -> FilePath -> TileFormat -> IO DeepZoomImage
createDZI workerId src dest tileFormat = do
    (exitCode, stdout, stderr) <- readProcessWithExitCode "vips" args ""
    logInfo "VIPS command"
      [ "command" .= show args
      , "exitCode" .= show exitCode
      , "stdout" .= stdout
      , "stderr" .= stderr
      , "worker" .= workerId
      ]
    when (exitCode /= ExitSuccess) $
      fail $ "VIPS error: Exit code: " ++ show exitCode ++
             ". stderr: " ++ stderr ++ ". stdout: " ++ stdout

    xml <- readFile dest
    case fromXML xml of
      (Just dzi) -> return dzi
      _ -> fail "Failed to create DZI"
  where
    args =
      [ "dzsave"
      , "--tile-size=" <> show TileSize254
      , "--overlap=" <> show TileOverlap1
      , src
      , dropExtension dest -- VIPS 7.38.5 automatically adds `.dzi` extension
      , "--suffix=" <> toVIPSSuffix tileFormat
      ]

uploadDZI :: String ->
             RackspaceConfig ->
             FilePath ->
             FilePath ->
             DeepZoomImage ->
             IO ()
uploadDZI workerId raxConfig rootPath path dzi = do
    meta <- getMetadata raxCreds
    tilePaths <- getDZITilePaths path

    -- Upload tiles
    let chunks = chunksOf numParallelUploads tilePaths
    sequence_ $ (`map` chunks) $ \chunk ->
      forConcurrently chunk $ \tilePath ->
        case toObjectName tilePath of
          (Just tileObjectName) -> do
            _ <- logDebugT "Upload DZI tile"
                  [ "container" .= container
                  , "objectName" .= tileObjectName
                  , "worker" .= workerId
                  ] $ putContent meta tilePath tileMIME container tileObjectName
            return ()

          Nothing -> logError "Invalid DZI tile object name"
                        [ "tilePath" .= tilePath
                        , "rootPath" .= rootPath
                        , "worker" .= workerId
                        ]

    -- Upload manifest
    case toObjectName path of
      (Just dziObjectName) -> do
        _ <- logDebugT "Upload DZI manifest"
              [ "container" .= container
              , "objectName" .= dziObjectName
              , "worker" .= workerId
              ] $ putContent meta path manifestMIME container dziObjectName
        return ()
      _ ->
        logError "Invalid DZI manifest object name"
          [ "path" .= path
          , "rootPath" .= rootPath
          , "worker" .= workerId
          ]
  where
    manifestMIME = MIME.Type (MIME.Application "xml") []
    tileMIME = toMIME (dziTileFormat dzi)
    container = raxContainer raxConfig
    containerPath = raxContainerPath raxConfig
    raxCreds = mkCredentials (raxUsername raxConfig) (raxApiKey raxConfig)
    numParallelUploads = 5

    stripRoot :: FilePath -> Maybe FilePath
    stripRoot = stripPrefix (addTrailingPathSeparator rootPath)

    toObjectName :: FilePath -> Maybe ObjectName
    toObjectName p =
      stripRoot p >>= Just . (show containerPath </>) >>= parseObjectName

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

toMIME :: TileFormat -> MIME.Type
toMIME JPEG = MIME.Type (MIME.Image "jpeg") []
toMIME PNG  = MIME.Type (MIME.Image "png") []

toVIPSSuffix :: TileFormat -> String
toVIPSSuffix PNG = ".png"
toVIPSSuffix JPEG = ".jpg[Q=90]"
