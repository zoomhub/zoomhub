{-# LANGUAGE FlexibleContexts #-}

module ZoomHub.AWS
  ( run,
  )
where

import qualified Amazonka as AWS
import qualified Amazonka.Auth as AWS
import Conduit (ResourceT)
import Control.Monad (when)
import Data.Binary.Builder (Builder)
import qualified Data.Binary.Builder as Builder
import qualified Data.ByteString.Lazy.UTF8 as BL
import Data.Functor ((<&>))
import UnliftIO (MonadUnliftIO)
import qualified ZoomHub.Config.AWS as Config
import qualified ZoomHub.Log.Logger as Logger

run ::
  (MonadUnliftIO m) =>
  Config.Config ->
  Logger.LogLevel ->
  (AWS.Env -> ResourceT m a) ->
  m a
run config logLevel action = do
  let accessKey = Config.configAccessKeyId config
      secretKey = Config.configSecretAccessKey config
  baseEnv <- AWS.newEnvNoAuth <&> AWS.fromKeys accessKey secretKey
  let env =
        baseEnv
          { AWS.logger = logger logLevel,
            AWS.region = Config.configRegion config
          }
  AWS.runResourceT (action env)

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
