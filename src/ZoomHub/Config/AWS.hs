{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Config.AWS
  ( Config (..),
    S3BucketName (..),
    fromEnv,
  )
where

import qualified Amazonka as AWS
import qualified Amazonka.Data as AWS
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import System.Environment (getEnvironment)
import ZoomHub.Utils (hush)

newtype S3BucketName = S3BucketName {unS3BucketName :: Text}
  deriving (Eq, Show)

data Config = Config
  { configAccessKeyId :: AWS.AccessKey,
    configSecretAccessKey :: AWS.SecretKey,
    configSourcesS3Bucket :: S3BucketName,
    configRegion :: AWS.Region
  }

fromEnv :: IO (Maybe Config)
fromEnv = do
  env <- getEnvironment
  return $ do
    accessKey <- AWS.AccessKey . encodeUtf8 . T.pack <$> lookup "AWS_ACCESS_KEY_ID" env
    secretKey <- AWS.SecretKey . encodeUtf8 . T.pack <$> lookup "AWS_SECRET_ACCESS_KEY" env
    region <- hush . AWS.fromText . T.pack =<< lookup "AWS_REGION" env
    sourcesS3Bucket <- S3BucketName . T.pack <$> lookup "S3_SOURCES_BUCKET" env
    pure $
      Config
        { configAccessKeyId = accessKey,
          configSecretAccessKey = secretKey,
          configSourcesS3Bucket = sourcesS3Bucket,
          configRegion = region
        }
