{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.IO.Class as IO
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Either as E
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Directory as SD
import qualified System.Environment as System
import qualified System.Envy as Env
import qualified Web.Hashids as H
import qualified ZoomHub.API as ZH
import qualified ZoomHub.Config as ZH

-- Main
main :: IO ()
main = do
  maybePort <- System.lookupEnv "PORT"
  raxConfig <- Env.decodeEnv
  case raxConfig of
    E.Right rackspace -> do
      lastId <- IO.liftIO $ STM.atomically $ STM.newTVar 0
      dataPath <- (++ "/data") <$> SD.getCurrentDirectory
      -- TODO: Move Hashid secret to config:
      let encodeContext = H.hashidsSimple "zoomhub hash salt"
          encodeIntegerId integerId =
            BSC.unpack $ H.encode encodeContext (fromIntegral integerId)
          port = maybe ZH.defaultPort read maybePort
          config = ZH.Config{..}
      Warp.run (fromIntegral port) (ZH.app config)
    E.Left message -> error $ "Failed to read environment: " ++ message
