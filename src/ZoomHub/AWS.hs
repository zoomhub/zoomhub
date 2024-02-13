{-# LANGUAGE FlexibleContexts #-}

module ZoomHub.AWS
  ( run,
  )
where

import Control.Monad (when)
import Data.Functor ((<&>))
import Data.Binary.Builder (Builder)
import Data.Typeable (Typeable)
import qualified Data.Binary.Builder as Builder
import qualified Data.ByteString.Lazy.UTF8 as BL
import Data.Text.Encoding (encodeUtf8)
import qualified Amazonka.Auth as AWS
import qualified Amazonka as AWS
import UnliftIO (MonadUnliftIO)
import qualified ZoomHub.Config.AWS as Config
import qualified ZoomHub.Log.Logger as Logger

run ::
  (MonadUnliftIO m, AWS.AWSRequest a, Typeable a, Typeable (AWS.AWSResponse a)) =>
  Config.Config ->
  Logger.LogLevel ->
  a ->
  m (AWS.AWSResponse a)
run config logLevel action = do
  let
    accessKey = AWS.AccessKey . encodeUtf8 . Config.configAccessKeyId $ config
    secretKey = AWS.SecretKey . encodeUtf8 . Config.configSecretAccessKey $ config
  -- baseEnv <- AWS.newEnv ()
  baseEnv <- AWS.newEnvNoAuth <&> AWS.fromKeys accessKey secretKey
  let
    env = baseEnv
          { AWS.logger = logger logLevel
          , AWS.region = Config.configRegion config
          }
  AWS.runResourceT $ AWS.send env action

logger :: Logger.LogLevel -> AWS.LogLevel -> Builder -> IO ()
logger minimumLogLevel logLevel builder =
  when (actualLogLevel >= minimumLogLevel) $
    Logger.log_ actualLogLevel (BL.toString . Builder.toLazyByteString $ builder)
  where
    actualLogLevel = level logLevel

    level AWS.Info = Logger.Info
    level AWS.Error = Logger.Error
    level AWS.Debug = Logger.Debug
    level AWS.Trace = Logger.Debug
