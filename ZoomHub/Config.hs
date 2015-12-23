{-# LANGUAGE DeriveGeneric #-}

module ZoomHub.Config
  ( Config(Config)
  , RackspaceConfig
  , defaultPort
  , lastId
  , port
  , rackspace
  , raxApiKey
  , raxUsername
  ) where

import qualified GHC.Generics as GHC
import qualified System.Envy as Env
import qualified Control.Concurrent.STM as STM


defaultPort :: Integer
defaultPort = 8000

data Config = Config
  { port :: Integer
  , lastId :: STM.TVar Integer
  , rackspace :: RackspaceConfig
  }

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
