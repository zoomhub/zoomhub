{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ZoomHub.Main (main) where

import           Control.Concurrent               (forkIO, threadDelay)
import           Control.Concurrent.STM           (TChan, TVar, atomically,
                                                   newTChan, newTVar, readTChan,
                                                   readTVar)
import           Control.Exception                (tryJust)
import           Control.Monad                    (forever, guard)
import           Control.Monad.IO.Class           (liftIO)
import qualified Data.ByteString.Char8            as BC
import           Network.Wai.Handler.Warp         (run)
import           System.AtomicWrite.Writer.String (atomicWriteFile)
import           System.Directory                 (getCurrentDirectory)
import           System.Environment               (lookupEnv)
import           System.Envy                      (decodeEnv)
import           System.FilePath.Posix            ((</>))
import           System.IO.Error                  (isDoesNotExistError)
import           Web.Hashids                      (encode, hashidsSimple)

import           ZoomHub.API                      (app)
import           ZoomHub.Config                   (Config (..), defaultPort)
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
  r <- tryJust doesNotExistGuard $ readFile (lastIdPath dataPath)
  return $ case r of
    Left _       -> 0
    Right lastId -> read lastId
  where doesNotExistGuard = guard . isDoesNotExistError

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

-- Main
main :: IO ()
main = do
  cd <- getCurrentDirectory
  maybePort <- lookupEnv "PORT"
  maybeDataPath <- lookupEnv "DATA_PATH"
  maybeHashidsSalt <- lookupEnv hashidsSaltEnvName
  maybeRaxConfig <- decodeEnv
  let defaultDataPath = cd </> "data"
      publicPath = cd </> "public"
      dataPath = maybe defaultDataPath id maybeDataPath
  case (maybeHashidsSalt, maybeRaxConfig) of
    (Just hashidsSalt, Right rackspace) -> do
      initialLastId <- readLastId dataPath
      lastId <- liftIO $ atomically $ newTVar initialLastId
      _ <- forkIO $ writeLastId dataPath lastId lastIdWriteInterval
      jobs <- liftIO $ atomically $ newTChan
      _ <- forkIO $ printJobs jobs lastIdWriteInterval
      let encodeContext = hashidsSimple $ BC.pack hashidsSalt
          encodeId integerId =
            BC.unpack $ encode encodeContext (fromIntegral integerId)
          port = maybe defaultPort read maybePort
          config = Config{..}
      putStrLn $ "Welcome to ZoomHub." ++
        " Go to <http://localhost:" ++ show port ++ "> and have fun!"
      run (fromIntegral port) (app config)
    (Nothing, _) -> error $ "Please set `" ++ hashidsSaltEnvName ++
      "` environment variable.\nThis secret salt enables ZoomHub to encode" ++
      " integer IDs as short, non-sequential string IDs which make it harder" ++
      " to guess valid content IDs."
    (_, Left message) -> error $ "Failed to read Rackspace config: " ++ message
