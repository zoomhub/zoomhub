{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ZoomHub.Main (main) where

import           Control.Concurrent                   (forkIO, threadDelay)
import           Control.Concurrent.STM               (TChan, TVar, atomically,
                                                       newTChan, newTVar,
                                                       readTChan, readTVar)
import           Control.Exception                    (tryJust)
import           Control.Monad                        (forever, guard)
import           Control.Monad.IO.Class               (liftIO)
import qualified Data.ByteString.Char8                as BC
import qualified Data.ByteString.Lazy                 as BL
import           Data.Default                         (def)
import           Data.Maybe                           (fromJust, fromMaybe)
import           Network.BSD                          (getHostName)
import           Network.URI                          (parseAbsoluteURI)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger (OutputFormat (CustomOutputFormatWithDetails),
                                                       mkRequestLogger,
                                                       outputFormat)
import           System.AtomicWrite.Writer.String     (atomicWriteFile)
import           System.Directory                     (getCurrentDirectory)
import           System.Environment                   (lookupEnv)
import           System.Envy                          (decodeEnv)
import           System.FilePath.Posix                ((</>))
import           System.IO.Error                      (isDoesNotExistError)
import           Web.Hashids                          (encode, hashidsSimple)

import           ZoomHub.API                          (app)
import           ZoomHub.Config                       (Config (..), defaultPort)
import           ZoomHub.Logger                       (formatAsJSON)
import           ZoomHub.Types.BaseURI                (BaseURI (BaseURI))
import           ZoomHub.Types.ContentBaseURI         (ContentBaseURI (ContentBaseURI))
-- import           ZoomHub.Pipeline                 (process)


-- TODO: Move to `Storage` module:
-- Last ID
lastIdPath :: FilePath -> FilePath
lastIdPath dataPath = dataPath </> "lastId.txt"

-- TODO: Figure out why `time-units` library doesn’t work:
lastIdWriteInterval :: Int
lastIdWriteInterval = 5 * 10^(6 :: Int) -- microseconds

readLastId :: String -> IO Integer
readLastId dataPath = do
  r <- tryJust (guard . isDoesNotExistError) $ readFile (lastIdPath dataPath)
  return $ case r of
    Left _       -> 0
    Right lastId -> read lastId

writeLastId :: String -> TVar Integer -> Int -> IO ()
writeLastId dataPath tvar interval = forever $ atomically (readTVar tvar)
  >>= \lastId -> atomicWriteFile (lastIdPath dataPath) (show lastId)
  >> threadDelay interval

-- Jobs
-- TODO: Implement job processing here:
printJobs :: TChan String -> Int -> IO ()
printJobs tchan interval = forever $ atomically (readTChan tchan)
  >>= \job -> putStrLn ("Job: " ++ job)
  >> threadDelay interval

-- Environment
hashidsSaltEnvName :: String
hashidsSaltEnvName = "HASHIDS_SALT"

baseURIEnvName :: String
baseURIEnvName = "BASE_URI"

-- Config
readVersion :: FilePath -> IO String
readVersion currentDirectory = do
  r <- tryJust (guard . isDoesNotExistError) $ readFile versionPath
  return $ case r of
    Left _        -> "unknown"
    Right version -> version
  where
    versionPath = currentDirectory </> "version.txt"

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
  maybeDataPath <- lookupEnv "DATA_PATH"
  maybePublicPath <- lookupEnv "PUBLIC_PATH"
  maybeHashidsSalt <- (fmap . fmap) BC.pack (lookupEnv hashidsSaltEnvName)
  maybeRaxConfig <- decodeEnv
  hostname <- getHostName
  maybeBaseURI <- lookupEnv baseURIEnvName
  logger <- mkRequestLogger def {
    outputFormat = CustomOutputFormatWithDetails formatAsJSON
  }
  let acceptNewContent = False
      defaultDataPath = currentDirectory </> "data"
      dataPath = fromMaybe defaultDataPath maybeDataPath
      port = maybe defaultPort read maybePort
      baseURI = case maybeBaseURI of
        Just uriString -> toBaseURI uriString
        Nothing        -> toBaseURI ("http://" ++ hostname)
      contentBaseURI = ContentBaseURI $
        fromJust . parseAbsoluteURI $ "http://content.zoomhub.net"
      defaultPublicPath = currentDirectory </> "public"
      publicPath = fromMaybe defaultPublicPath maybePublicPath
  case (maybeHashidsSalt, maybeRaxConfig) of
    (Just hashidsSalt, Right rackspace) -> do
      initialLastId <- readLastId dataPath
      lastId <- liftIO $ atomically $ newTVar initialLastId
      _ <- forkIO $ writeLastId dataPath lastId lastIdWriteInterval
      jobs <- liftIO $ atomically newTChan
      _ <- forkIO $ printJobs jobs lastIdWriteInterval
      let encodeContext = hashidsSimple hashidsSalt
          encodeId integerId =
            BC.unpack $ encode encodeContext (fromIntegral integerId)
          config = Config{..}
      putStrLn $ "{\"message\": \"Welcome to ZoomHub." ++
        " Go to <" ++ show baseURI ++ "> and have fun!\"}"
      run (fromIntegral port) (app config)
    (Nothing, _) -> error $ "Please set `" ++ hashidsSaltEnvName ++
      "` environment variable.\nThis secret salt enables ZoomHub to encode" ++
      " integer IDs as short, non-sequential string IDs which make it harder" ++
      " to guess valid content IDs."
    (_, Left message) -> error $ "Failed to read Rackspace config: " ++ message
  where
    toBaseURI :: String -> BaseURI
    toBaseURI uriString =
      case parseAbsoluteURI uriString of
        Just uri -> BaseURI uri
        Nothing  -> error $ "'" ++ uriString ++ "' is not a valid URL. Please\
        \ set `" ++ baseURIEnvName ++ "` to override usage of hostname."
