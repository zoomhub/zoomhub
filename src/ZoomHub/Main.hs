{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Main (main) where

import qualified Control.Concurrent as C
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as Ex
import qualified Control.Monad as M
import qualified Control.Monad.IO.Class as IO
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Either as E
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.AtomicWrite.Writer.String as A
import qualified System.Directory as SD
import qualified System.Environment as System
import qualified System.Envy as Env
import qualified System.IO.Error as System
import qualified Web.Hashids as H
import qualified ZoomHub.API as ZH
import qualified ZoomHub.Config as ZH
import qualified ZoomHub.Rackspace.CloudFiles as CF


lastIdPath :: String -> String
lastIdPath dataPath = dataPath ++ "/lastId.txt"

-- TODO: Figure out why `time-units` library doesn’t work:
lastIdWriteInterval :: Int
lastIdWriteInterval = 5 * 10^(6 :: Int) -- microseconds

readLastId :: String -> IO Integer
readLastId dataPath = do
  r <- Ex.tryJust (M.guard . System.isDoesNotExistError) $ readFile (lastIdPath dataPath)
  return $ read $ case r of
    Left _       -> "0"
    Right lastId -> lastId

writeLastId :: String -> STM.TVar Integer -> Int -> IO ()
writeLastId dataPath tvar interval = M.forever $ STM.atomically (STM.readTVar tvar)
  >>= \x -> A.atomicWriteFile (lastIdPath dataPath) (show x)
  >> C.threadDelay interval

-- Main
main :: IO ()
main = do
  maybePort <- System.lookupEnv "PORT"
  raxConfig <- Env.decodeEnv
  case raxConfig of
    E.Right rackspace -> do
      -- TODO: Initialize from `/data/lastId.txt`:
      dataPath <- (++ "/data") <$> SD.getCurrentDirectory
      initialLastId <- readLastId dataPath
      lastId <- IO.liftIO $ STM.atomically $ STM.newTVar initialLastId
      _ <- C.forkIO $ writeLastId dataPath lastId lastIdWriteInterval
      -- TODO: Move Hashid secret to config:
      let encodeContext = H.hashidsSimple "zoomhub hash salt"
          encodeIntegerId integerId =
            BSC.unpack $ H.encode encodeContext (fromIntegral integerId)
          port = maybe ZH.defaultPort read maybePort
          config = ZH.Config{..}
      let username = ZH.raxUsername . ZH.rackspace $ config
          apiKey = ZH.raxApiKey . ZH.rackspace $ config
          creds = CF.Credentials username apiKey
      meta <- CF.getMetadata creds
      Warp.run (fromIntegral port) (ZH.app config meta)
    E.Left message -> error $ "Failed to read environment: " ++ message
