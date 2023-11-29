{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Config.AWS
  ( Config (..),
    S3BucketName (..),
    fromEnv,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Amazonka as AWS
import System.Environment (getEnvironment)

newtype S3BucketName = S3BucketName {unS3BucketName :: Text}
  deriving (Eq, Show)

data Config = Config
  { configAccessKeyId :: Text,
    configSecretAccessKey :: Text,
    configSourcesS3Bucket :: S3BucketName,
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
      <*> (S3BucketName . T.pack <$> lookup "S3_SOURCES_BUCKET" env)
      <*> Just region
