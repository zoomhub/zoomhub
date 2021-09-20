{-# LANGUAGE FlexibleContexts #-}

module ZoomHub.AWS
  ( run,
  )
where

import Control.Lens ((&), (.~))
import Control.Monad.Catch (MonadCatch)
import Data.Text.Encoding (encodeUtf8)
import Network.AWS (AccessKey (AccessKey), Credentials (FromKeys), SecretKey (SecretKey))
import qualified Network.AWS as AWS
import System.IO (stdout)
import UnliftIO (MonadUnliftIO)
import qualified ZoomHub.Config.AWS as Config

run :: (MonadCatch m, MonadUnliftIO m) => Config.Config -> AWS.AWS a -> m a
run config action = do
  logger <- AWS.newLogger AWS.Debug stdout
  env <-
    AWS.newEnv $
      FromKeys
        (AccessKey . encodeUtf8 . Config.configAccessKeyId $ config)
        (SecretKey . encodeUtf8 . Config.configSecretAccessKey $ config)
  AWS.runResourceT
    . AWS.runAWS (env & AWS.envLogger .~ logger)
    . AWS.within (Config.configRegion config)
    $ action
