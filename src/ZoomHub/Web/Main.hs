{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Web.Main
  ( main,
  )
where

import Control.Concurrent (getNumCapabilities, threadDelay)
import Control.Concurrent.Async (async)
import Control.Exception (SomeException, tryJust)
import Control.Monad (forM_, guard, when)
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as BL
import Data.Default (def)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import Data.Time.Units (Second, toMicroseconds)
import Data.Time.Units.Instances ()
import GHC.Conc (getNumProcessors)
import qualified Network.AWS as AWS
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
import System.Directory (createDirectoryIfMissing, getCurrentDirectory)
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
import ZoomHub.Config.ProcessContent (ProcessContent (..))
import qualified ZoomHub.Config.ProcessContent as ProcessContent
import ZoomHub.Config.Uploads (Uploads (..))
import qualified ZoomHub.Config.Uploads as Uploads
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
import ZoomHub.Types.TempPath (TempPath (TempPath), unTempPath)
import ZoomHub.Worker (processExistingContent, processExpiredActiveContent)

-- Environment variables
baseURIEnvName :: String
baseURIEnvName = "BASE_URI"

contentBaseURIEnvName :: String
contentBaseURIEnvName = "CONTENT_BASE_URI"

staticBaseURIEnvName :: String
staticBaseURIEnvName = "STATIC_BASE_URI"

processContentEnvName :: String
processContentEnvName = "PROCESS_CONTENT"

uploadsEnvName :: String
uploadsEnvName = "UPLOADS"

numProcessingWorkersEnvName :: String
numProcessingWorkersEnvName = "PROCESSING_WORKERS"

portEnvName :: String
portEnvName = "PORT"

publicPathEnvName :: String
publicPathEnvName = "PUBLIC_PATH"

tempRootPathEnvName :: String
tempRootPathEnvName = "TEMP_PATH"

-- Main
main :: IO ()
main = do
  -- TODO: Migrate configuration to `configurator`:
  -- https://hackage.haskell.org/package/configurator
  env <- getEnvironment
  hostname <- getHostName
  environment <-
    fromMaybe
      (error "ZoomHub.Main: Missing `ZH_ENV`")
      <$> Environment.fromEnv
  currentDirectory <- getCurrentDirectory
  openSeadragonScript <-
    readFile $
      currentDirectory
        </> "public"
        </> "lib"
        </> "openseadragon"
        </> "openseadragon.min.js"
  error404 <- BL.readFile $ currentDirectory </> "public" </> "404.html"
  version <- readVersion currentDirectory
  logger <- mkRequestLogger $ def {outputFormat = CustomOutputFormatWithDetails formatAsJSON}
  numProcessors <- getNumProcessors
  numCapabilities <- getNumCapabilities
  aws <-
    fromMaybe
      (error "ZoomHub.Main: Failed to parse AWS configuration.")
      <$> AWSConfig.fromEnv AWS.Ohio -- TODO: Grab AWS region from environment?
  let port = fromMaybe defaultPort (lookup portEnvName env >>= readMaybe)
      maybeProcessContent = ProcessContent.parse <$> lookup processContentEnvName env
      processContent = fromMaybe ProcessNoContent maybeProcessContent
      maybeUploads = Uploads.parse <$> lookup uploadsEnvName env
      uploads = fromMaybe UploadsDisabled maybeUploads
      defaultNumProcessingWorkers = 0 :: Integer
      maybeNumProcessingWorkers = lookup numProcessingWorkersEnvName env >>= readMaybe
      numProcessingWorkers = fromMaybe defaultNumProcessingWorkers maybeNumProcessingWorkers
      defaultPublicPath = currentDirectory </> "public"
      publicPath = fromMaybe defaultPublicPath (lookup publicPathEnvName env)
      defaultTempRootPath = currentDirectory </> "data"
      tempPath = TempPath $ fromMaybe defaultTempRootPath (lookup tempRootPathEnvName env) </> "temp"
      baseURI = case lookup baseURIEnvName env of
        Just uriString ->
          toBaseURI uriString
        Nothing ->
          toBaseURI $ "http://" <> hostname
      defaultStaticBaseURI = StaticBaseURI . fromJust . parseAbsoluteURI $ "http://static.zoomhub.net"
      mStaticBaseURI = StaticBaseURI <$> (parseAbsoluteURI =<< lookup staticBaseURIEnvName env)
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
  dbConnInfo <- ConnectInfo.fromEnv defaultDBName
  dbConnPool <-
    createConnectionPool
      dbConnInfo
      dbConnPoolNumStripes
      dbConnPoolIdleTime
      dbConnPoolMaxResourcesPerStripe
  let config = Config {..}
  ensureTempPathExists tempPath
  logInfo_ $
    "Welcome to ZoomHub.\
    \ Go to <"
      ++ show baseURI
      ++ "> and have fun!"
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
            "'" ++ uriString
              ++ "' is not a valid URL. Please\
                 \ set `"
              ++ baseURIEnvName
              ++ "` to override usage of hostname."
    ensureTempPathExists :: TempPath -> IO ()
    ensureTempPathExists tempPath =
      createDirectoryIfMissing True rawTempPath
      where
        rawTempPath = unTempPath tempPath
    readVersion :: FilePath -> IO String
    readVersion currentDirectory = do
      r <- tryJust (guard . isDoesNotExistError) $ readFile versionPath
      return $ case r of
        Left _ -> "unknown"
        Right version -> version
      where
        versionPath = currentDirectory </> "version.txt"
    serverExceptionHandler :: Maybe Request -> SomeException -> IO ()
    serverExceptionHandler _ e =
      when (defaultShouldDisplayException e) $
        logException_ "Web server exception" e
