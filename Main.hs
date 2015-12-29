{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}

import qualified Control.Concurrent.STM as STM
import qualified Control.Monad.IO.Class as IO
import qualified Data.Either as E
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Environment as System
import qualified System.Envy as Env
import qualified ZoomHub.API as ZH
import qualified ZoomHub.Config as C

-- Main
main :: IO ()
main = do
  maybePort <- System.lookupEnv "PORT"
  raxConfig <- Env.decodeEnv
  case raxConfig of
    E.Right rackspace -> do
      lastId <- IO.liftIO $ STM.atomically $ STM.newTVar 0
      let port = maybe C.defaultPort read maybePort
      let config = C.Config{..}
      Warp.run (fromIntegral port) (ZH.app config)
    E.Left message -> error $ "Failed to read environment: " ++ message
