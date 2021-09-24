{-# LANGUAGE FlexibleContexts #-}

module ZoomHub.AWS
  ( run,
  )
where

import Control.Lens ((&), (.~))
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch)
import Data.Binary.Builder (Builder)
import qualified Data.Binary.Builder as Builder
import qualified Data.ByteString.Lazy.UTF8 as BL
import Data.Text.Encoding (encodeUtf8)
import Network.AWS (AccessKey (AccessKey), Credentials (FromKeys), SecretKey (SecretKey))
import qualified Network.AWS as AWS
import UnliftIO (MonadUnliftIO)
import qualified ZoomHub.Config.AWS as Config
import qualified ZoomHub.Log.Logger as Logger

run ::
  (MonadCatch m, MonadUnliftIO m) =>
  Config.Config ->
  Logger.LogLevel ->
  AWS.AWS a ->
  m a
run config logLevel action = do
  env <-
    AWS.newEnv $
      FromKeys
        (AccessKey . encodeUtf8 . Config.configAccessKeyId $ config)
        (SecretKey . encodeUtf8 . Config.configSecretAccessKey $ config)
  AWS.runResourceT
    . AWS.runAWS (env & AWS.envLogger .~ logger logLevel)
    . AWS.within (Config.configRegion config)
    $ action

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
