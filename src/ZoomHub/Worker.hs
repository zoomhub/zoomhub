{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ZoomHub.Worker
  ( processExistingContent,
    processExpiredActiveContent,
  )
where

import Control.Concurrent (threadDelay)
import Control.Exception.Enclosed (catchAny)
import Control.Lens ((&), (.~), (^.))
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy (toStrict)
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Time.Units (Second, fromMicroseconds, toMicroseconds)
import Data.Time.Units.Instances ()
import qualified Network.AWS as AWS
import qualified Network.AWS.Lambda as AWSLambda
import Squeal.PostgreSQL.Pool (runPoolPQ)
import System.Random (randomRIO)
import qualified ZoomHub.AWS as ZHAWS
import ZoomHub.Config (Config (..))
import ZoomHub.Log.Logger (logException, logInfo)
import ZoomHub.Storage.PostgreSQL (dequeueNextUnprocessed)
import ZoomHub.Types.Content (Content (contentId))
import ZoomHub.Types.ContentId (unContentId)
import qualified ZoomHub.Types.Environment as Environment
import ZoomHub.Utils (lenientDecodeUtf8)

-- Constants
processExistingContentInterval :: Second
processExistingContentInterval = 5

-- Public API
processExistingContent :: Config -> String -> IO ()
processExistingContent Config {..} workerId = forever $ do
  -- logDebug "worker:start" ["worker" .= workerId]
  go `catchAny` \ex ->
    -- TODO: Mark as `completed:failure` or `initialized`:
    logException "worker:error" ex extraLogMeta
  -- logDebug "worker:end" extraLogMeta
  let delta = (2 * toMicroseconds sleepBase) `div` 2
  jitter <- randomRIO (0, delta)
  let sleepDuration = fromMicroseconds $ delta + jitter :: Second
  -- logDebug "Wait for next unprocessed content" $
  --   ("sleepDuration" .= sleepDuration) : extraLogMeta
  threadDelay . fromIntegral $ toMicroseconds sleepDuration
  where
    go = do
      mContent <- liftIO $ runPoolPQ dequeueNextUnprocessed dbConnPool
      for_ mContent $ \content -> do
        logInfo
          "worker:lambda:start"
          [ "wwwURL" .= wwwURL content,
            "apiURL" .= apiURL content
          ]
        ZHAWS.run aws logLevel $ do
          response <-
            AWS.send $
              AWSLambda.invoke
                "processContent"
                (toStrict . encode $ object ["contentURL" .= apiURL content])
                & AWSLambda.iQualifier .~ (Just . Environment.toText $ environment)
          for_ (response ^. AWSLambda.irsPayload) $ \output ->
            liftIO $
              logInfo
                "worker:lambda:response"
                [ "output" .= lenientDecodeUtf8 output,
                  "wwwURL" .= wwwURL content,
                  "apiURL" .= apiURL content
                ]
    sleepBase = processExistingContentInterval
    -- TODO: Split `BASE_URI` into `WWW_BASE_URI` and `API_BASE_URI`:
    wwwURL c = mconcat [show baseURI, "/", unContentId (contentId c)]
    apiURL c = mconcat [show baseURI, "/v1/content/", unContentId (contentId c)]
    extraLogMeta =
      [ "worker" .= workerId,
        "topic" .= ("worker:process:existing" :: Text)
      ]

processExpiredActiveContent :: Config -> IO ()
processExpiredActiveContent Config {..} = pure ()
