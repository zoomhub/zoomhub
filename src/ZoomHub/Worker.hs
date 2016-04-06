{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Worker
  ( processExistingContent
  ) where

import           Control.Concurrent     (threadDelay)
import           Control.Monad          (forever)
import           Data.Aeson             ((.=))

import           ZoomHub.Config         (Config)
import qualified ZoomHub.Config         as Config
import           ZoomHub.Log.Logger     (logDebug)
import           ZoomHub.Pipeline       (process)
import           ZoomHub.Storage.SQLite (getNextUnprocessed)

-- TODO: Figure out why `time-units` library doesn’t work:
toMicroseconds :: Int -> Int
toMicroseconds = (* 10^(6 :: Int))

processExistingContentInterval :: Int
processExistingContentInterval = 3

processExistingContent :: Config -> IO ()
processExistingContent config = forever $ do
  maybeContent <- getNextUnprocessed (Config.dbConnection config)
  _ <- case maybeContent of
    Just content -> do
      completedContent <- process config content
      return $ Just completedContent
    Nothing      -> return Nothing
  logDebug "Wait for next unprocessed content"
    ["sleepDuration" .= processExistingContentInterval]
  threadDelay (toMicroseconds processExistingContentInterval)
