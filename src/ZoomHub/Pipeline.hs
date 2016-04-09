{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Pipeline
  ( process
  )
  where

import           Codec.MIME.Parse                         (parseMIMEType)
import qualified Codec.MIME.Type                          as MIME
import           Control.Concurrent.Async                 (forConcurrently)
import           Control.Exception                        (SomeException, catch,
                                                           tryJust, throwIO)
import           Control.Lens                             ((^.))
import           Data.Aeson                               ((.=))
import           Data.List                                (stripPrefix)
import           Data.Monoid                              ((<>))
import qualified Data.Text                                as T
import           Data.Text.Encoding                       (decodeUtf8)
import           Database.SQLite.Simple                   (Connection)
import           Network.HTTP.Client                     (HttpException (FailedConnectionException2))
import           Network.Wreq                             (get, responseBody,
                                                           responseHeader)
import           System.AtomicWrite.Writer.LazyByteString (atomicWriteFile)
import           System.Directory                         (createDirectoryIfMissing,
                                                           listDirectory)
import           System.FilePath                          (addTrailingPathSeparator,
                                                           dropExtension, (<.>),
                                                           (</>))
import           System.IO.Temp                           (withTempDirectory)
import           System.Posix                             (fileSize,
                                                           getFileStatus)
import           System.Process                           (callProcess)

import           ZoomHub.Config                           (Config,
                                                           RackspaceConfig,
                                                           raxApiKey,
                                                           raxContainer,
                                                           raxUsername)
import qualified ZoomHub.Config                           as Config
import           ZoomHub.Log.Logger                       (logDebug, logError,
                                                           logInfo)
import           ZoomHub.Rackspace.CloudFiles             (ObjectName,
                                                           getMetadata,
                                                           mkCredentials,
                                                           parseObjectName,
                                                           putContent)
import           ZoomHub.Storage.SQLite                   (markAsActive,
                                                           markAsFailure,
                                                           markAsSuccess,
                                                           withConnection)
import           ZoomHub.Types.Content                    (Content, contentId,
                                                           contentURL)
import           ZoomHub.Types.ContentId                  (unId)
import           ZoomHub.Types.ContentMIME                (ContentMIME (ContentMIME))
import           ZoomHub.Types.ContentURI                 (ContentURI)
import           ZoomHub.Types.DeepZoomImage              (DeepZoomImage, TileFormat (JPEG, PNG), TileOverlap (TileOverlap1), TileSize (TileSize254),
                                                           dziTileFormat,
                                                           fromXML)

process :: Config -> Content -> IO Content
process config content =
  withConnection (Config.dbPath config) $ \dbConn ->
    unsafeProcess config dbConn content `catch` \e -> do
      let errorMessage = Just . T.pack . show $ (e :: SomeException)
      logInfo "Process content: failure"
        [ "id" .= contentId content
        , "error" .= errorMessage
        ]
      markAsFailure dbConn content errorMessage

unsafeProcess :: Config -> Connection -> Content -> IO Content
unsafeProcess config dbConn content = do
  let tempPath = Config.dataPath config </> "content-raw"
  createDirectoryIfMissing True tempPath

  withTempDirectory tempPath template $ \tmpDir -> do
    let rawPathPrefix = tmpDir </> rawContentId
        rawPath = rawPathPrefix ++ "-raw"
        dziPath = rawPathPrefix <.> "dzi"

    logInfo "Create temporary working directory"
      [ "id" .= contentId content
      , "path" .= tempPath
      ]

    logInfo "Mark content as active"
      [ "id" .= contentId content ]
    activeContent <- markAsActive dbConn content

    logInfo "Download content"
      [ "id" .= contentId content
      , "url" .= contentURL content
      ]
    maybeMIME <-
      ((<$>) . (<$>)) ContentMIME $ downloadURL (contentURL content) rawPath
    rawSize <- getFileSize rawPath

    logInfo "Create DZI"
      [ "id" .= contentId content ]
    dzi <- createDZI rawPath dziPath (toTileFormat maybeMIME)

    logInfo "Content metadata"
      [ "mime" .= maybeMIME
      , "size" .= rawSize
      , "dzi" .= dzi
      ]

    logInfo "Upload DZI"
      [ "id" .= contentId content
      , "dzi" .= dzi
      , "dziPath" .= dziPath
      ]
    uploadDZI raxConfig tmpDir dziPath dzi

    logInfo "Process content: success"
      [ "id" .= contentId activeContent ]
    markAsSuccess dbConn activeContent dzi maybeMIME rawSize
  where
    raxConfig = Config.rackspace config
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

createDZI :: FilePath -> FilePath -> TileFormat -> IO DeepZoomImage
createDZI src dest tileFormat = do
    logDebug "VIPS command" [ "command" .= T.pack (show args) ]
    callProcess "vips" args
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

uploadDZI :: RackspaceConfig ->
             FilePath ->
             FilePath ->
             DeepZoomImage ->
             IO ()
uploadDZI raxConfig rootPath path dzi = do
    meta <- getMetadata raxCreds
    tilePaths <- getDZITilePaths path

    -- Upload Tiles
    _ <- forConcurrently tilePaths $ \tilePath ->
      case toObjectName tilePath of
        (Just tileObjectName) -> do
          logDebug "Upload DZI tile"
            [ "container" .= container
            , "objectName" .= tileObjectName
            ]
          r <- tryJust isFailedConnectionException2 $
            putContent meta tilePath tileMIME container tileObjectName
          case r of
            Left e@(FailedConnectionException2 host port secure cause) -> do
              logError "Upload DZI tile: Failed to connect to server"
                [ "container" .= container
                , "objectName" .= tileObjectName
                , "host" .= host
                , "port" .= port
                , "secure" .= secure
                , "httpError" .= show e
                , "cause" .= show cause
                ]
              throwIO e
            Left e ->
              logError "Upload DZI tile: Unknown HTTP error"
                [ "container" .= container
                , "objectName" .= tileObjectName
                , "httpError" .= show e
                ]
            Right _ -> return ()
        Nothing -> logError "Invalid DZI tile object name"
              [ "tilePath" .= tilePath
              , "rootPath" .= rootPath
              ]

    -- Upload manifest
    case toObjectName path of
      (Just dziObjectName) -> do
        logDebug "Upload DZI manifest"
          [ "container" .= container
          , "objectName" .= dziObjectName
          ]
        _ <- putContent meta path manifestMIME container dziObjectName
        return ()
      _ ->
        logError "Invalid DZI manifest object name"
          [ "path" .= path
          , "rootPath" .= rootPath
          ]
  where
    manifestMIME = MIME.Type (MIME.Application "xml") []
    tileMIME = toMIME (dziTileFormat dzi)
    container = raxContainer raxConfig
    raxCreds = mkCredentials (raxUsername raxConfig) (raxApiKey raxConfig)

    stripRoot :: FilePath -> Maybe FilePath
    stripRoot = stripPrefix (addTrailingPathSeparator rootPath)

    isFailedConnectionException2 :: HttpException -> Maybe HttpException
    isFailedConnectionException2 e@(FailedConnectionException2 _ _ _ _) = Just e
    isFailedConnectionException2 _ = Nothing

    -- TODO: Make `content` prefix configurable:
    objectNamePrefix = "content"

    toObjectName :: FilePath -> Maybe ObjectName
    toObjectName p = stripRoot p >>= Just . (objectNamePrefix </>)
      >>= parseObjectName

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
