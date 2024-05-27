{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ZoomHub.Worker
  ( processExistingContent,
    processExpiredActiveContent,
  )
where

import qualified Amazonka as AWS
import qualified Amazonka.Lambda as AWSLambda
import qualified Amazonka.Lambda.Lens as AWSLambda
import Control.Concurrent (threadDelay)
import Control.Exception.Enclosed (catchAny)
import Control.Lens ((&), (.~), (^.))
import Control.Monad (forever)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode, object, (.=))
import Data.ByteString.Lazy (toStrict)
import Data.Foldable (for_)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8Lenient)
import Data.Time.Units (Second, fromMicroseconds, toMicroseconds)
import Data.Time.Units.Instances ()
import Squeal.PostgreSQL.Session.Pool (usingConnectionPool)
import System.Random (randomRIO)
import qualified ZoomHub.AWS as ZHAWS
import ZoomHub.Config (Config (..))
import ZoomHub.Log.Logger (logException, logInfo)
import ZoomHub.Storage.PostgreSQL (dequeueNextUnprocessed)
import ZoomHub.Types.Content (Content (contentId))
import ZoomHub.Types.ContentId (unContentId)
import qualified ZoomHub.Types.Environment as Environment

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
      mContent <- liftIO $ usingConnectionPool dbConnPool dequeueNextUnprocessed
      for_ mContent $ \content -> do
        logInfo
          "worker:lambda:start"
          [ "wwwURL" .= wwwURL content,
            "apiURL" .= apiURL content
          ]
        ZHAWS.run aws logLevel $ \env -> do
          response <-
            AWS.send env $
              AWSLambda.newInvoke
                "processContent"
                (toStrict . encode $ object ["contentURL" .= apiURL content])
                & AWSLambda.invoke_qualifier .~ (Just . Environment.toText $ environment)
          for_ (response ^. AWSLambda.invokeResponse_payload) $ \output ->
            liftIO $
              logInfo
                "worker:lambda:response"
                [ "output" .= decodeUtf8Lenient output,
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
processExpiredActiveContent Config {} = pure ()
