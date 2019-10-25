{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Main (main) where

import Control.Concurrent (getNumCapabilities, threadDelay)
import Control.Concurrent.Async (async)
import Control.Exception (SomeException, tryJust)
import Control.Monad (forM_, guard, unless, when)
import Data.Aeson ((.=))
import qualified Data.ByteString.Lazy as BL
import Data.Default (def)
import Data.Maybe (fromJust, fromMaybe)
import Data.Time.Units (Second, toMicroseconds)
import Data.Time.Units.Instances ()
import GHC.Conc (getNumProcessors)
import Network.BSD (getHostName)
import Network.URI (parseAbsoluteURI)
import Network.Wai (Request)
import Network.Wai.Handler.Warp
  ( defaultSettings
  , defaultShouldDisplayException
  , runSettings
  , setOnException
  , setPort
  )
import Network.Wai.Middleware.RequestLogger
  (OutputFormat(CustomOutputFormatWithDetails), mkRequestLogger, outputFormat)
import System.Directory
  (createDirectoryIfMissing, doesFileExist, getCurrentDirectory)
import System.Environment (getEnvironment)
import System.Envy (decodeEnv)
import System.FilePath ((</>))
import System.IO.Error (isDoesNotExistError)
import System.Random (randomRIO)
import Text.Read (readMaybe)

import ZoomHub.API (app)
import ZoomHub.Config
  ( Config(..)
  , ExistingContentStatus(IgnoreExistingContent, ProcessExistingContent)
  , NewContentStatus(NewContentDisallowed)
  , defaultPort
  , raxContainer
  , raxContainerPath
  , toExistingContentStatus
  , toNewContentStatus
  )
import ZoomHub.Log.Logger (logException_, logInfo, logInfo_)
import ZoomHub.Log.RequestLogger (formatAsJSON)
import ZoomHub.Rackspace.CloudFiles (unContainer)
import ZoomHub.Storage.PostgreSQL2 (createConnectionPool)
import qualified ZoomHub.Storage.PostgreSQL2 as ConnectInfo (fromEnv)
import ZoomHub.Types.BaseURI (BaseURI(BaseURI))
import ZoomHub.Types.ContentBaseURI (mkContentBaseURI)
import ZoomHub.Types.DatabasePath (DatabasePath(DatabasePath), unDatabasePath)
import ZoomHub.Types.StaticBaseURI (StaticBaseURI(StaticBaseURI))
import ZoomHub.Types.TempPath (TempPath(TempPath), unTempPath)
import ZoomHub.Worker (processExistingContent, processExpiredActiveContent)


-- Environment variables
baseURIEnvName :: String
baseURIEnvName = "BASE_URI"

dbPathEnvName :: String
dbPathEnvName = "DB_PATH"

existingContentStatusEnvName :: String
existingContentStatusEnvName = "PROCESS_EXISTING_CONTENT"

newContentStatusEnvName :: String
newContentStatusEnvName = "PROCESS_NEW_CONTENT"

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
  maybeRaxConfig <- decodeEnv
  hostname <- getHostName
  currentDirectory <- getCurrentDirectory
  openSeadragonScript <- readFile $ currentDirectory </>
    "public" </> "lib" </> "openseadragon" </> "openseadragon.min.js"
  error404 <- BL.readFile $ currentDirectory </> "public" </> "404.html"
  version <- readVersion currentDirectory
  logger <- mkRequestLogger def
              { outputFormat = CustomOutputFormatWithDetails formatAsJSON }

  numProcessors <- getNumProcessors
  numCapabilities <- getNumCapabilities

  let port = fromMaybe defaultPort (lookup portEnvName env >>= readMaybe)

      maybeExistingContentStatus = toExistingContentStatus <$>
        lookup existingContentStatusEnvName env
      existingContentStatus =
        fromMaybe IgnoreExistingContent maybeExistingContentStatus

      maybeNewContentStatus = toNewContentStatus <$>
        lookup newContentStatusEnvName env
      newContentStatus =
        fromMaybe NewContentDisallowed maybeNewContentStatus

      defaultNumProcessingWorkers = 0 :: Integer
      maybeNumProcessingWorkers =
        lookup numProcessingWorkersEnvName env >>= readMaybe
      numProcessingWorkers =
        fromMaybe defaultNumProcessingWorkers maybeNumProcessingWorkers

      defaultDBPath = DatabasePath $
        currentDirectory </> "data" </> "zoomhub-development.sqlite3"
      dbPath = maybe defaultDBPath DatabasePath (lookup dbPathEnvName env)

      defaultPublicPath = currentDirectory </> "public"
      publicPath = fromMaybe defaultPublicPath (lookup publicPathEnvName env)

      defaultTempRootPath = currentDirectory </> "data"
      tempPath = TempPath $ fromMaybe defaultTempRootPath
        (lookup tempRootPathEnvName env) </> "temp"

      baseURI = case lookup baseURIEnvName env of
        Just uriString -> toBaseURI uriString
        Nothing        -> toBaseURI ("http://" ++ hostname)

      staticBaseURI = StaticBaseURI . fromJust .
        parseAbsoluteURI $ "http://static.zoomhub.net"

      defaultDBName = "zoomhub_development"

      -- Database connection pool:
      -- https://github.com/brettwooldridge/HikariCP/wiki/About-Pool-Sizing#the-formula
      numSpindles = 1
      dbConnPoolNumStripes = 1
      dbConnPoolIdleTime = 10 :: Second
      dbConnPoolMaxResourcesPerStripe = fromIntegral $
        (numCapabilities * 2) + numSpindles

  dbConnInfo <- ConnectInfo.fromEnv defaultDBName
  dbConnPool <- createConnectionPool dbConnInfo dbConnPoolNumStripes
    dbConnPoolIdleTime dbConnPoolMaxResourcesPerStripe

  ensureTempPathExists tempPath
  ensureDBExists dbPath

  case maybeRaxConfig of
    Right rackspace -> do

      let maybeContentBaseHost = parseAbsoluteURI $
            "http://" ++ unContainer (raxContainer rackspace) ++ ".zoomhub.net"
          contentBasePath = raxContainerPath rackspace
          maybeContentBaseURI = maybeContentBaseHost >>=
            \baseHost -> mkContentBaseURI baseHost contentBasePath
          contentBaseURI =
            case maybeContentBaseURI of
              Just uri -> uri
              _ -> error "ZoomHub.Main: Failed to parse `contentBaseURI`."

          config = Config{..}

      logInfo_ $ "Welcome to ZoomHub.\
        \ Go to <" ++ show baseURI ++ "> and have fun!"
      logInfo "Config: App"
        [ "config" .= config ]

      -- Workers
      logInfo "Config: Worker"
        [ "numProcessors" .= numProcessors
        , "numCapabilities" .= numCapabilities
        , "numProcessingWorkers" .= numProcessingWorkers
        , "numProcessExpiredActiveWorkers" .= (1 :: Integer)
        ]

      _ <- async $ do
        let delay = 30 :: Second
        logInfo "Worker: Schedule resetting expired active content"
          [ "delay" .= delay ]
        threadDelay (fromIntegral $ toMicroseconds delay)
        processExpiredActiveContent config

      case existingContentStatus of
        ProcessExistingContent ->
          forM_ [0 .. (numProcessingWorkers - 1)] $ \index -> async $ do
            let base = 20
                jitterRange = (0, base `div` 2) :: (Integer, Integer)
                baseDelay = index * base
            jitter <- randomRIO jitterRange
            let delay = (fromIntegral $ baseDelay + jitter) :: Second
            logInfo "Worker: Start processing existing content"
              [ "jitter" .= (fromIntegral jitter :: Second)
              , "index" .= index
              , "delay" .= delay
              ]
            threadDelay (fromIntegral $ toMicroseconds delay)
            processExistingContent config (show index)
        IgnoreExistingContent -> return ()

      -- Web server
      logInfo "Start web server"
        [ "port" .= port ]
      let waiSettings =
            setPort (fromIntegral port) $
            setOnException serverExceptionHandler defaultSettings
      runSettings waiSettings (app config)

    Left message ->
      error $ "Failed to read Rackspace config: " ++ message
  where
    toBaseURI :: String -> BaseURI
    toBaseURI uriString =
      case parseAbsoluteURI uriString of
        Just uri -> BaseURI uri
        Nothing  -> error $ "'" ++ uriString ++ "' is not a valid URL. Please\
        \ set `" ++ baseURIEnvName ++ "` to override usage of hostname."

    ensureDBExists :: DatabasePath -> IO ()
    ensureDBExists dbPath = do
      exists <- doesFileExist (unDatabasePath dbPath)
      unless exists $
        error $ "Couldn’t find a database at " ++ unDatabasePath dbPath ++
          ". Please check `" ++ dbPathEnvName ++ "`."

    ensureTempPathExists :: TempPath -> IO ()
    ensureTempPathExists tempPath =
        createDirectoryIfMissing True rawTempPath
      where
        rawTempPath = unTempPath tempPath

    readVersion :: FilePath -> IO String
    readVersion currentDirectory = do
      r <- tryJust (guard . isDoesNotExistError) $ readFile versionPath
      return $ case r of
        Left _        -> "unknown"
        Right version -> version
      where
        versionPath = currentDirectory </> "version.txt"

    serverExceptionHandler :: Maybe Request -> SomeException -> IO ()
    serverExceptionHandler _ e =
      when (defaultShouldDisplayException e) $
        logException_ "Web server exception" e
