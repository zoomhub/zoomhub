{-# LANGUAGE DeriveGeneric #-}

import qualified Data.Either as E
import qualified Data.Maybe as M
import qualified GHC.Generics as GHC
import qualified Network.Wai.Handler.Warp as Warp
import qualified System.Environment as System
import qualified System.Envy as Env
import qualified ZoomHub.API as ZoomHub
import qualified ZoomHub.Rackspace.CloudFiles as CF


-- Config: Rackspace
data RackspaceConfig = RackspaceConfig
  { raxUsername :: String -- RACKSPACE_USERNAME
  , raxApiKey :: String   -- RACKSPACE_API_KEY
  } deriving (GHC.Generic, Show)

-- Default configuration will be used for fields that could not be
-- retrieved from the environment:
instance Env.DefConfig RackspaceConfig where
  defConfig = RackspaceConfig{raxUsername="zoomingservice", raxApiKey=""}

instance Env.FromEnv RackspaceConfig where
  fromEnv = Env.gFromEnvCustom Env.Option
    { Env.dropPrefixCount=3
    , Env.customPrefix="RACKSPACE"
    }

-- Main
defaultPort :: Int
defaultPort = 8000

main :: IO ()
main = do
  maybePort <- System.lookupEnv "PORT"
  raxConfig <- Env.decodeEnv
  case raxConfig of
    E.Right config ->
      let port = read $ M.fromMaybe (show defaultPort) maybePort in
      Warp.run port (ZoomHub.app credentials)
      where
        username = raxUsername config
        apiKey = raxApiKey config
        credentials = CF.Credentials username apiKey
    E.Left message -> error $ "Failed to read environment: " ++ message
