{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Worker
  ( processExistingContent
  , processExpiredActiveContent
  ) where

import           Control.Concurrent         (threadDelay)
import           Control.Exception.Enclosed (catchAny)
import           Control.Monad              (forever)
import           Data.Aeson                 ((.=))
import qualified Data.Text                  as T
import           Data.Time.Units            (Minute, Second, fromMicroseconds,
                                             toMicroseconds)
import           Data.Time.Units.Instances  ()
import           System.Random              (randomRIO)

import           ZoomHub.Config             (Config)
import qualified ZoomHub.Config             as Config
import           ZoomHub.Log.Logger         (logException, logInfo, logInfoT)
import           ZoomHub.Pipeline           (process)
import           ZoomHub.Storage.SQLite     (dequeueNextUnprocessed,
                                             getExpiredActive, markAsFailure,
                                             markAsSuccess, resetAsInitialized,
                                             withConnection)
import           ZoomHub.Types.Content      (contentId)

-- Constants
processExistingContentInterval :: Minute
processExistingContentInterval = 1

processExpiredActiveContentInterval :: Minute
processExpiredActiveContentInterval = 30

-- Public API
processExistingContent :: Config -> String -> IO ()
processExistingContent config workerId = forever $ do
    logInfo "worker:start" [ "worker" .= workerId ]

    go `catchAny` \ex -> do
      -- TODO: Mark as `completed:failure` or `initialized`:
      logException "worker:error" ex extraLogMeta

    logInfo "worker:end" extraLogMeta

    let delta = (2 * (toMicroseconds sleepBase)) `div` 2
    jitter <- randomRIO (0, delta)
    let sleepDuration = fromMicroseconds $ delta + jitter :: Second

    logInfo "Wait for next unprocessed content" $
      [ "sleepDuration" .= sleepDuration ] ++ extraLogMeta
    threadDelay . fromIntegral $ toMicroseconds sleepDuration
  where
    go = withConnection dbPath $ \dbConn -> do
      maybeContent <-
        logT "Get next unprocessed content and mark as active" [] $
          dequeueNextUnprocessed dbConn

      case maybeContent of
        Just content ->
          logT "Process content: success"
            [ "id" .= contentId content ] $ do
              processed <-
                process workerId raxConfig tempPath content `catchAny` \e -> do
                  let errorMessage = Just . T.pack $ show e
                  logT "Process content: failure"
                    [ "id" .= contentId content
                    , "error" .= errorMessage
                    ] $ markAsFailure dbConn content errorMessage

              _ <- markAsSuccess dbConn processed
              return ()
        Nothing -> return ()

    dbPath = Config.dbPath config
    raxConfig = Config.rackspace config
    tempPath = Config.tempPath config

    sleepBase = processExistingContentInterval

    logT msg meta = logInfoT msg (meta ++ extraLogMeta)
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
