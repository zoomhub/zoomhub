{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Worker
  ( processExistingContent
  , processExpiredActiveContent
  ) where

import           Control.Exception.Enclosed               (catchAny)
import           Control.Concurrent        (threadDelay)
import           Control.Monad             (forever)
import           Data.Aeson                ((.=))
import           Data.Time.Units           (Minute, Second, toMicroseconds)
import           Data.Time.Units.Instances ()
import qualified Data.Text as T

import           ZoomHub.Config            (Config)
import qualified ZoomHub.Config            as Config
import           ZoomHub.Log.Logger        (logInfoT, logInfo)
import           ZoomHub.Pipeline          (process)
import           ZoomHub.Storage.SQLite    (getExpiredActive,
                                            dequeueNextUnprocessed,
                                            resetAsInitialized,
                                            withConnection,
                                            markAsSuccess,
                                            markAsFailure)
import           ZoomHub.Types.Content     (contentId)

-- Constants
processExistingContentInterval :: Second
processExistingContentInterval = 3

processExpiredActiveContentInterval :: Minute
processExpiredActiveContentInterval = 30

-- Public API
processExistingContent :: Config -> String -> IO ()
processExistingContent config workerId = forever $
  withConnection dbPath $ \dbConn -> do

    maybeContent <- logT "Get next unprocessed content and mark as active" [] $
      dequeueNextUnprocessed dbConn

    case maybeContent of
      Just content -> do
        logT "Process content: success"
          [ "id" .= contentId content ] $ do
            processed <- process raxConfig tempPath content `catchAny` \e -> do
              let errorMessage = Just . T.pack $ show e
              logT "Process content: failure"
                [ "id" .= contentId content
                , "error" .= errorMessage
                ] $ markAsFailure dbConn content errorMessage

            _ <- markAsSuccess dbConn processed
            return ()
      Nothing -> return ()

    logInfo "Wait for next unprocessed content" $
      [ "sleepDuration" .= sleepDuration ] ++ extraLogMeta

    threadDelay . fromIntegral $ toMicroseconds sleepDuration
  where
    dbPath = Config.dbPath config
    raxConfig = Config.rackspace config
    tempPath = Config.tempPath config

    sleepDuration = processExistingContentInterval

    logT msg meta action = logInfoT msg (meta ++ extraLogMeta) action
    extraLogMeta =
      [ "worker" .= workerId
      , "topic" .= ("worker:process:existing" :: T.Text)
      ]

processExpiredActiveContent :: Config -> IO ()
processExpiredActiveContent config = forever $
  withConnection (Config.dbPath config) $ \dbConn -> do
    cs <- getExpiredActive dbConn
    logInfoT "Reset expired active content"
      [ "ids" .= map contentId cs ]
      (resetAsInitialized dbConn cs)

    logInfo "Wait for next expired active content"
      [ "sleepDuration" .= sleepDuration ]
    threadDelay . fromIntegral $ toMicroseconds sleepDuration
  where
    sleepDuration = processExpiredActiveContentInterval
