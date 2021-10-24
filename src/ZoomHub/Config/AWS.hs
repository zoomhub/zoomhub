{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Config.AWS
  ( Config (..),
    fromEnv,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Network.AWS as AWS
import System.Environment (getEnvironment)

data Config = Config
  { configAccessKeyId :: Text,
    configSecretAccessKey :: Text,
    configSourcesS3Bucket :: Text,
    configRegion :: AWS.Region
  }

fromEnv :: AWS.Region -> IO (Maybe Config)
fromEnv region = do
  env <- getEnvironment
  -- TODO: Refactor to use named instead of positional arguments:
  return $
    Config
      <$> (T.pack <$> lookup "AWS_ACCESS_KEY_ID" env)
      <*> (T.pack <$> lookup "AWS_SECRET_ACCESS_KEY" env)
      <*> (T.pack <$> lookup "ZH_S3_SOURCES_BUCKET" env)
      <*> Just region
