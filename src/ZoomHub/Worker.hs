{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Worker
  ( processExistingContent
  ) where

import           Control.Concurrent     (threadDelay)
import           Control.Monad          (forever)
import           Data.Aeson             ((.=))
import           Data.Time.Units        (Minute, Second,
                                         TimeUnit, toMicroseconds)

import           ZoomHub.Config         (Config)
import qualified ZoomHub.Config         as Config
import           ZoomHub.Log.Logger     (logDebug)
import           ZoomHub.Pipeline       (process)
import           ZoomHub.Storage.SQLite (getNextUnprocessed)


processExistingContentInterval :: Second
processExistingContentInterval = 3

processExpiredActiveContentInterval :: Minute
processExpiredActiveContentInterval = 30

processExistingContent :: Config -> IO ()
processExistingContent config = forever $ do
    maybeContent <- getNextUnprocessed (Config.dbConnection config)
    case maybeContent of
      Just content -> do
        _ <- process config content
        return ()
      _ -> return ()

    logDebug "Wait for next unprocessed content"
      [ "sleepDuration" .= show sleepDuration ]
    threadDelay . fromIntegral $ toMicroseconds sleepDuration
  where
    sleepDuration = processExistingContentInterval
