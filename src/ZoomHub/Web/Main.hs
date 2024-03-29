{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Web.Main
  ( webMain,
  )
where

import Control.Concurrent (getNumCapabilities, threadDelay)
import Control.Concurrent.Async (async)
import Control.Exception (SomeException, tryJust)
import Control.Monad (forM_, guard, when)
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as BL
import Data.Default (def)
import Data.Functor ((<&>))
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Units (Second, toMicroseconds)
import Data.Time.Units.Instances ()
import Flow
import GHC.Conc (getNumProcessors)
import Network.HostName (getHostName)
import Network.URI (parseAbsoluteURI)
import Network.Wai (Request)
import Network.Wai.Handler.Warp
  ( defaultSettings,
    defaultShouldDisplayException,
    runSettings,
    setOnException,
    setPort,
  )
import Network.Wai.Middleware.RequestLogger
  ( OutputFormat (CustomOutputFormatWithDetails),
    mkRequestLogger,
    outputFormat,
  )
import System.Directory (getCurrentDirectory)
import System.Environment (getEnvironment)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import System.Random (randomRIO)
import Text.Read (readMaybe)
import ZoomHub.API (app)
import ZoomHub.Config
  ( Config (..),
    defaultPort,
  )
import qualified ZoomHub.Config.AWS as AWSConfig
import qualified ZoomHub.Config.Kinde as Kinde
import ZoomHub.Config.ProcessContent (ProcessContent (..))
import qualified ZoomHub.Config.ProcessContent as ProcessContent
import ZoomHub.Config.Uploads (Uploads (..))
import qualified ZoomHub.Config.Uploads as Uploads
import qualified ZoomHub.Log.LogLevel as LogLevel
import ZoomHub.Log.Logger (logException_, logInfo, logInfo_)
import ZoomHub.Log.RequestLogger (formatAsJSON)
import ZoomHub.Storage.PostgreSQL (createConnectionPool)
import qualified ZoomHub.Storage.PostgreSQL as ConnectInfo (fromEnv)
import ZoomHub.Types.APIUser (APIUser (APIUser))
import qualified ZoomHub.Types.APIUser as APIUser
import ZoomHub.Types.BaseURI (BaseURI (BaseURI))
import ZoomHub.Types.ContentBaseURI (mkContentBaseURI)
import qualified ZoomHub.Types.Environment as Environment
import ZoomHub.Types.StaticBaseURI (StaticBaseURI (StaticBaseURI))
import ZoomHub.Worker (processExistingContent, processExpiredActiveContent)

-- Environment variables
baseURIEnvName :: String
baseURIEnvName = "BASE_URI"

contentBaseURIEnvName :: String
contentBaseURIEnvName = "CONTENT_BASE_URI"

-- Main
webMain :: IO ()
webMain = do
  -- TODO: Migrate configuration to `configurator`:
  -- https://hackage.haskell.org/package/configurator
  env <- getEnvironment
  hostname <- getHostName
  environment <-
    fromMaybe
      (error "ZoomHub.Main: Missing `ZH_ENV`")
      <$> Environment.fromEnv
  currentDirectory <- getCurrentDirectory
  let defaultPublicPath = currentDirectory </> "frontend" </> "public"
  let publicPath = fromMaybe defaultPublicPath (lookup "PUBLIC_PATH" env)
  openSeadragonScript <-
    readFile $
      publicPath
        </> "lib"
        </> "openseadragon"
        </> "openseadragon.min.js"
  error404 <- BL.readFile $ publicPath </> "404.html"
  version <- readVersion currentDirectory
  logger <- mkRequestLogger $ def {outputFormat = CustomOutputFormatWithDetails formatAsJSON}
  numProcessors <- getNumProcessors
  numCapabilities <- getNumCapabilities
  aws <- AWSConfig.fromEnv <&> fromMaybe (error "ZoomHub.Main: Failed to parse AWS configuration.")
  kinde <- Kinde.fromEnv <&> fromMaybe (error "ZoomHub.Main: Failed to parse Kinde configuration.")
  let logLevel = fromMaybe LogLevel.Debug $ lookup "LOG_LEVEL" env >>= LogLevel.parse
  let port = fromMaybe defaultPort (lookup "PORT" env >>= readMaybe)
      maybeProcessContent = ProcessContent.parse <$> lookup "PROCESS_CONTENT" env
      processContent = fromMaybe ProcessNoContent maybeProcessContent
      maybeUploads = Uploads.parse <$> lookup "UPLOADS" env
      uploads = fromMaybe UploadsDisabled maybeUploads
      maxUploadSizeMegabytes = fromMaybe 50 (lookup "UPLOADS_MAX_SIZE_MEGABYTES" env >>= readMaybe)
      defaultNumProcessingWorkers = 0 :: Integer
      maybeNumProcessingWorkers = lookup "PROCESSING_WORKERS" env >>= readMaybe
      numProcessingWorkers = fromMaybe defaultNumProcessingWorkers maybeNumProcessingWorkers
      baseURI = case lookup baseURIEnvName env of
        Just uriString ->
          toBaseURI uriString
        Nothing ->
          toBaseURI $ "http://" <> hostname
      defaultStaticBaseURI = StaticBaseURI . fromJust . parseAbsoluteURI $ "https://static.zoomhub.net"
      mStaticBaseURI = StaticBaseURI <$> (parseAbsoluteURI =<< lookup "STATIC_BASE_URI" env)
      staticBaseURI = fromMaybe defaultStaticBaseURI mStaticBaseURI
      defaultDBName = "zoomhub_development"
      -- Database connection pool:
      -- https://github.com/brettwooldridge/HikariCP/wiki/About-Pool-Sizing#the-formula
      numSpindles = 1
      dbConnPoolNumStripes = 1
      dbConnPoolIdleTime = 10 :: Second
      dbConnPoolMaxResourcesPerStripe = fromIntegral $ (numCapabilities * 2) + numSpindles
      contentBaseURI =
        fromMaybe
          (error $ "ZoomHub.Main: Failed to parse content base URI. Please check '" <> contentBaseURIEnvName <> "'.")
          (mkContentBaseURI =<< parseAbsoluteURI =<< lookup contentBaseURIEnvName env)
      apiUser =
        fromMaybe
          (error "ZoomHub.Main: Missing API user. Please set 'API_USERNAME' and/or 'API_PASSWORD'.")
          ( do
              username <- T.pack <$> lookup "API_USERNAME" env
              password <- T.pack <$> lookup "API_PASSWORD" env
              pure $ APIUser {..}
          )
  -- Database connection pool
  dbConnInfo <- ConnectInfo.fromEnv defaultDBName
  dbConnPool <-
    createConnectionPool
      dbConnInfo
      dbConnPoolNumStripes
      dbConnPoolIdleTime
      dbConnPoolMaxResourcesPerStripe
  logInfo
    "Config: Database connection pool"
    [ "dbConnPoolNumStripes" .= dbConnPoolNumStripes,
      "dbConnPoolIdleTime" .= dbConnPoolIdleTime,
      "dbConnPoolMaxResourcesPerStripe" .= dbConnPoolMaxResourcesPerStripe
    ]

  let config = Config {..}
  logInfo_ $ "Welcome to ZoomHub. Go to <" <> show baseURI <> "> and have fun!"
  logInfo
    "Config: App"
    ["config" .= config]
  -- Workers
  logInfo
    "Config: Worker"
    [ "numProcessors" .= numProcessors,
      "numCapabilities" .= numCapabilities,
      "numProcessingWorkers" .= numProcessingWorkers,
      "numProcessExpiredActiveWorkers" .= (1 :: Integer)
    ]
  _ <- async $ do
    let delay = 30 :: Second
    logInfo
      "Worker: Schedule resetting expired active content"
      ["delay" .= delay]
    threadDelay (fromIntegral $ toMicroseconds delay)
    processExpiredActiveContent config
  case processContent of
    ProcessExistingContent ->
      startProcessingWorkers config numProcessingWorkers
    ProcessExistingAndNewContent ->
      startProcessingWorkers config numProcessingWorkers
    ProcessNoContent ->
      return ()
  -- Web server
  logInfo
    "Start web server"
    ["port" .= port]
  let waiSettings =
        setPort (fromIntegral port)
          . setOnException serverExceptionHandler
          $ defaultSettings
  waiApp <- app config
  runSettings waiSettings waiApp
  where
    startProcessingWorkers :: Config -> Integer -> IO ()
    startProcessingWorkers config numProcessingWorkers = do
      forM_ [0 .. (numProcessingWorkers - 1)] $ \index -> async $ do
        let base = 20
            jitterRange = (0, base `div` 2) :: (Integer, Integer)
            baseDelay = index * base
        jitter <- randomRIO jitterRange
        let delay = (fromIntegral $ baseDelay + jitter) :: Second
        logInfo
          "Worker: Start processing existing content"
          [ "jitter" .= (fromIntegral jitter :: Second),
            "index" .= index,
            "delay" .= delay
          ]
        threadDelay (fromIntegral $ toMicroseconds delay)
        processExistingContent config (show index)

    toBaseURI :: String -> BaseURI
    toBaseURI uriString =
      case parseAbsoluteURI uriString of
        Just uri -> BaseURI uri
        Nothing ->
          error $
            "'"
              <> uriString
              <> "' is not a valid URL.\
                 \ Please set `"
              <> baseURIEnvName
              <> "` to override usage of hostname."

    readVersion :: FilePath -> IO Text
    readVersion currentDirectory = do
      r <- tryJust (guard . isDoesNotExistError) $ readFile versionPath
      return $ case r of
        Left _ -> "<unknown>"
        Right version -> version |> T.pack
      where
        versionPath = currentDirectory </> "version.txt"

    serverExceptionHandler :: Maybe Request -> SomeException -> IO ()
    serverExceptionHandler _ e =
      when (defaultShouldDisplayException e) $
        logException_ "Web server exception" e
