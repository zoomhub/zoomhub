{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ZoomHub.Main (main) where

import           Control.Concurrent                   (getNumCapabilities,
                                                       threadDelay)
import           Control.Concurrent.Async             (async)
import           Control.Exception                    (SomeException, tryJust)
import           Control.Monad                        (forM_, guard, unless,
                                                       when)
import           Data.Aeson                           ((.=))
import qualified Data.ByteString.Char8                as BC
import qualified Data.ByteString.Lazy                 as BL
import           Data.Default                         (def)
import           Data.Maybe                           (fromJust, fromMaybe)
import           Data.Time.Units                      (Second, toMicroseconds)
import           GHC.Conc                             (getNumProcessors)
import           Network.BSD                          (getHostName)
import           Network.URI                          (parseAbsoluteURI)
import           Network.Wai                          (Request)
import           Network.Wai.Handler.Warp             (defaultSettings, defaultShouldDisplayException,
                                                       runSettings,
                                                       setOnException, setPort)
import           Network.Wai.Middleware.RequestLogger (OutputFormat (CustomOutputFormatWithDetails),
                                                       mkRequestLogger,
                                                       outputFormat)
import           System.Directory                     (createDirectoryIfMissing,
                                                       doesFileExist,
                                                       getCurrentDirectory)
import           System.Environment                   (lookupEnv)
import           System.Envy                          (decodeEnv)
import           System.FilePath                      ((</>))
import           System.IO.Error                      (isDoesNotExistError)
import           System.Random                        (randomRIO)
import           Web.Hashids                          (encode, hashidsSimple)

import           ZoomHub.API                          (app)
import           ZoomHub.Config                       (Config (..), ExistingContentStatus (ProcessExistingContent, IgnoreExistingContent), NewContentStatus (NewContentDisallowed),
                                                       defaultPort,
                                                       raxContainer,
                                                       toExistingContentStatus,
                                                       toNewContentStatus)
import           ZoomHub.Log.Logger                   (logException_, logInfo,
                                                       logInfo_)
import           ZoomHub.Log.RequestLogger            (formatAsJSON)
import           ZoomHub.Rackspace.CloudFiles         (unContainer)
import           ZoomHub.Types.BaseURI                (BaseURI (BaseURI))
import           ZoomHub.Types.ContentBaseURI         (ContentBaseURI (ContentBaseURI))
import           ZoomHub.Types.DatabasePath           (DatabasePath (DatabasePath),
                                                       unDatabasePath)
import           ZoomHub.Types.StaticBaseURI          (StaticBaseURI (StaticBaseURI))
import           ZoomHub.Types.TempPath               (TempPath (TempPath),
                                                       unTempPath)
import           ZoomHub.Types.Time.Instances         ()
import           ZoomHub.Worker                       (processExistingContent, processExpiredActiveContent)

-- Environment
baseURIEnvName :: String
baseURIEnvName = "BASE_URI"

dbPathEnvName :: String
dbPathEnvName = "DB_PATH"

existingContentStatusEnvName :: String
existingContentStatusEnvName = "PROCESS_EXISTING_CONTENT"

hashidsSaltEnvName :: String
hashidsSaltEnvName = "HASHIDS_SALT"

newContentStatusEnvName :: String
newContentStatusEnvName = "PROCESS_NEW_CONTENT"

tempRootPathEnvName :: String
tempRootPathEnvName = "TEMP_PATH"

-- Main
main :: IO ()
main = do
  -- TODO: Migrate configuration to `configurator`:
  -- https://hackage.haskell.org/package/configurator
  currentDirectory <- getCurrentDirectory
  openseadragonScript <- readFile $ currentDirectory </>
    "public" </> "lib" </> "openseadragon" </> "openseadragon.min.js"
  error404 <- BL.readFile $ currentDirectory </> "public" </> "404.html"
  version <- readVersion currentDirectory
  maybePort <- lookupEnv "PORT"
  maybeRootTempPath <- lookupEnv tempRootPathEnvName
  maybeDBPath <- (fmap . fmap) DatabasePath (lookupEnv dbPathEnvName)
  maybePublicPath <- lookupEnv "PUBLIC_PATH"
  maybeHashidsSalt <- (fmap . fmap) BC.pack (lookupEnv hashidsSaltEnvName)
  maybeRaxConfig <- decodeEnv
  hostname <- getHostName
  maybeBaseURI <- lookupEnv baseURIEnvName
  maybeExistingContentStatus <- (fmap . fmap) toExistingContentStatus
    (lookupEnv existingContentStatusEnvName)
  maybeNewContentStatus <- (fmap . fmap) toNewContentStatus
    (lookupEnv newContentStatusEnvName)
  logger <- mkRequestLogger def {
    outputFormat = CustomOutputFormatWithDetails formatAsJSON
  }
  let existingContentStatus =
        fromMaybe IgnoreExistingContent maybeExistingContentStatus
      newContentStatus = fromMaybe NewContentDisallowed maybeNewContentStatus
      defaultTempRootPath = currentDirectory </> "data"
      defaultDBPath = DatabasePath $
        currentDirectory </> "data" </> "zoomhub-development.sqlite3"
      tempPath =
        TempPath $ fromMaybe defaultTempRootPath maybeRootTempPath </> "temp"
      dbPath = fromMaybe defaultDBPath maybeDBPath
      port = maybe defaultPort read maybePort
      baseURI = case maybeBaseURI of
        Just uriString -> toBaseURI uriString
        Nothing        -> toBaseURI ("http://" ++ hostname)
      staticBaseURI = StaticBaseURI . fromJust .
        parseAbsoluteURI $ "http://static.zoomhub.net"
      defaultPublicPath = currentDirectory </> "public"
      publicPath = fromMaybe defaultPublicPath maybePublicPath

  ensureTempPathExists tempPath
  ensureDBExists dbPath

  case (maybeHashidsSalt, maybeRaxConfig) of
    (Just hashidsSalt, Right rackspace) -> do

      let encodeContext = hashidsSimple hashidsSalt
          encodeId integerId =
            BC.unpack $ encode encodeContext (fromIntegral integerId)
          contentBaseURI = ContentBaseURI . fromJust . parseAbsoluteURI $
            "http://" ++ unContainer (raxContainer rackspace) ++ ".zoomhub.net"
          config = Config{..}
      logInfo_ $ "Welcome to ZoomHub.\
        \ Go to <" ++ show baseURI ++ "> and have fun!"
      logInfo "Config: App"
        [ "config" .= config ]

      -- Workers
      numProcessors <- getNumProcessors
      numCapabilities <- getNumCapabilities
      let numProcessingWorkers = max (numCapabilities - 1) 1
      logInfo "Config: Worker"
        [ "numProcessors" .= numProcessors
        , "numCapabilities" .= numCapabilities
        , "numProcessingWorkers" .= numProcessingWorkers
        , "numProcessExpiredActiveWorkers" .= (1 :: Integer)
        ]

      _ <- async $ do
        let delay = (30 :: Second)
        logInfo "Worker: Start resetting expired active content"
          [ "delay" .= delay ]
        threadDelay (fromIntegral $ toMicroseconds delay)
        processExpiredActiveContent config

      case existingContentStatus of
        ProcessExistingContent -> do
          forM_ [0 .. (numProcessingWorkers - 1)] $ \index -> async $ do
            let base = 10
                jitterRange = (0, base `div` 2) :: (Int, Int)
                baseDelay = index * base
            jitter <- randomRIO jitterRange
            let delay = (fromIntegral $ baseDelay + jitter) :: Second
            logInfo "Worker: Start processing existing content"
              [ "jitter" .= jitter
              , "index" .= index
              , "delay" .= delay
              ]
            threadDelay (fromIntegral $ toMicroseconds delay)
            processExistingContent config (show index)

          return ()
        _ -> return ()

      -- Web server
      logInfo "Start web server"
        [ "port" .= port ]
      let waiSettings =
            setPort (fromIntegral port) $
            setOnException exceptionHandler defaultSettings
      runSettings waiSettings (app config)
    (Nothing, _) -> error $ "Please set `" ++ hashidsSaltEnvName ++ "`\
      \ environment variable.\n\
      \This secret salt enables ZoomHub to encode integer IDs as short,\
      \ non-sequential string IDs which make it harder to guess valid\
      \ content IDs."
    (_, Left message) -> error $ "Failed to read Rackspace config: " ++ message
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
    ensureTempPathExists tempPath = do
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

    exceptionHandler :: Maybe Request -> SomeException -> IO ()
    exceptionHandler _ e =
      when (defaultShouldDisplayException e) $
        logException_ "Web server exception" e
