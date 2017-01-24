{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ZoomHub.Worker
  ( processExistingContent
  , processExpiredActiveContent
  ) where

import           Control.Concurrent            (threadDelay)
import           Control.Exception             (SomeException, fromException)
import           Control.Exception.Enclosed    (catchAny)
import           Control.Monad                 (forever, void)
import           Data.Aeson                    (Value (String), encode, toJSON,
                                                (.=))
import qualified Data.ByteString.Lazy          as BL
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import           Data.Time.Units               (Minute, Second,
                                                fromMicroseconds,
                                                toMicroseconds)
import           Data.Time.Units.Instances     ()
import           Network.HTTP.Client           (HttpException)
import           Network.HTTP.Client.Instances ()
import           System.Random                 (randomRIO)

import           ZoomHub.Config                (Config)
import qualified ZoomHub.Config                as Config
import           ZoomHub.Log.Logger            (logException, logInfo, logInfoT)
import           ZoomHub.Pipeline              (process)
import           ZoomHub.Storage.SQLite        (dequeueNextUnprocessed,
                                                getExpiredActive, markAsFailure,
                                                markAsSuccess,
                                                resetAsInitialized,
                                                withConnection)
import           ZoomHub.Types.Content         (contentId, contentURL)
import           ZoomHub.Types.ContentId       (unContentId)
import           ZoomHub.Utils                 (lenientDecodeUtf8)

-- Constants
processExistingContentInterval :: Minute
processExistingContentInterval = 1

processExpiredActiveContentInterval :: Minute
processExpiredActiveContentInterval = 30

-- Public API
processExistingContent :: Config -> String -> IO ()
processExistingContent config workerId = forever $ do
    logInfo "worker:start" [ "worker" .= workerId ]

    go `catchAny` \ex ->
      -- TODO: Mark as `completed:failure` or `initialized`:
      logException "worker:error" ex extraLogMeta

    logInfo "worker:end" extraLogMeta

    let delta = (2 * toMicroseconds sleepBase) `div` 2
    jitter <- randomRIO (0, delta)
    let sleepDuration = fromMicroseconds $ delta + jitter :: Second

    logInfo "Wait for next unprocessed content" $
      ( "sleepDuration" .= sleepDuration ) : extraLogMeta
    threadDelay . fromIntegral $ toMicroseconds sleepDuration
  where
    go = withConnection dbPath $ \dbConn -> do
      maybeContent <-
        logT "Get next unprocessed content and mark as active" [] $
          dequeueNextUnprocessed dbConn

      case maybeContent of
        Just content -> do
          let processOp = process workerId raxConfig tempPath content >>=
                          markAsSuccess dbConn
              jsonToText =
                Just . lenientDecodeUtf8 . BL.toStrict . encode . toJSONError
              errorOp e = markAsFailure dbConn content (jsonToText e)
              handler e = logT "Process content: failure"
                            [ "id" .= contentId content
                            , "url" .= contentURL content
                            , "apiURL" .= apiURL content
                            , "wwwURL" .= wwwURL content
                            , "error" .= toJSONError e
                            , "worker" .= workerId
                            ] $ errorOp e
          logT "Process content: success"
            [ "id" .= contentId content
            , "url" .= contentURL content
            , "apiURL" .= apiURL content
            , "wwwURL" .= wwwURL content
            , "worker" .= workerId
            ] $ void (processOp `catchAny` handler)
        Nothing -> return ()

    dbPath = Config.dbPath config
    raxConfig = Config.rackspace config
    tempPath = Config.tempPath config

    sleepBase = processExistingContentInterval

    wwwURL c = "http://zoomhub.net/" ++ unContentId (contentId c)
    apiURL c = "http://zoomhub.net/v1/content/" ++ unContentId (contentId c)

    logT msg meta = logInfoT msg (meta ++ extraLogMeta)
    extraLogMeta =
      [ "worker" .= workerId
      , "topic" .= ("worker:process:existing" :: Text)
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

toJSONError :: SomeException -> Value
toJSONError e =
  case fromException e of
    Just (h :: HttpException) -> toJSON h
    _ -> String . T.pack . show $ e
