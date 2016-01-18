{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ZoomHub.Main (main) where

import           Control.Concurrent               (forkIO, threadDelay)
import           Control.Concurrent.STM           (TVar, atomically, newTVar,
                                                   readTVar)
import           Control.Exception                (tryJust)
import           Control.Monad                    (forever, guard)
import           Control.Monad.IO.Class           (liftIO)
import qualified Data.ByteString.Char8            as BC
import           Data.Either                      (Either (Left, Right))
import           Network.Wai.Handler.Warp         (run)
import           System.AtomicWrite.Writer.String (atomicWriteFile)
import           System.Directory                 (getCurrentDirectory)
import           System.Environment               (lookupEnv)
import           System.Envy                      (decodeEnv)
import           System.FilePath.Posix            ((</>))
import           System.IO.Error                  (isDoesNotExistError)
import           Web.Hashids                      (encode, hashidsSimple)

import           ZoomHub.API                      (app)
import           ZoomHub.Config                   (Config (..), RackspaceConfig,
                                                   defaultPort, raxApiKey,
                                                   raxUsername, raxUsername)

lastIdPath :: String -> String
lastIdPath dataPath = dataPath ++ "/lastId.txt"

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
  >>= \x -> atomicWriteFile (lastIdPath dataPath) (show x)
  >> threadDelay interval

-- See: https://mail.haskell.org/pipermail/libraries/2010-April/013417.html
(<$$>) :: (Functor f) => f a -> (a -> b) -> f b
(<$$>) = flip (<$>)

-- Main
main :: IO ()
main = do
  maybePort <- lookupEnv "PORT"
  raxConfig <- decodeEnv
  case raxConfig of
    Right rackspace -> do
      dataPath <- getCurrentDirectory <$$> (</> "data")
      initialLastId <- readLastId dataPath
      lastId <- liftIO $ atomically $ newTVar initialLastId
      _ <- forkIO $ writeLastId dataPath lastId lastIdWriteInterval
      -- TODO: Move Hashid secret to config:
      let encodeContext = hashidsSimple "zoomhub hash salt"
          encodeId integerId =
            BC.unpack $ encode encodeContext (fromIntegral integerId)
          port = maybe defaultPort read maybePort
          config = Config{..}
      run (fromIntegral port) (app config)
    Left message -> error $ "Failed to read environment: " ++ message
