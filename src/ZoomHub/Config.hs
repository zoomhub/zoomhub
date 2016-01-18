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
  , dataPath
  , encodeId
  ) where

import           Control.Concurrent.STM (TVar)
import           GHC.Generics           (Generic)
import           System.Envy            (DefConfig, FromEnv, Option (..),
                                         customPrefix, defConfig,
                                         dropPrefixCount, fromEnv,
                                         gFromEnvCustom)


defaultPort :: Integer
defaultPort = 8000

data Config = Config
  { port      :: Integer
  , lastId    :: TVar Integer
  , rackspace :: RackspaceConfig
  , dataPath  :: String
  , encodeId  :: Integer -> String
  }

-- Config: Rackspace
data RackspaceConfig = RackspaceConfig
  { raxUsername :: String -- RACKSPACE_USERNAME
  , raxApiKey   :: String -- RACKSPACE_API_KEY
  } deriving (Generic, Show)

-- Default configuration will be used for fields that could not be
-- retrieved from the environment:
instance DefConfig RackspaceConfig where
  defConfig = RackspaceConfig{raxUsername="zoomingservice", raxApiKey=""}

instance FromEnv RackspaceConfig where
  fromEnv = gFromEnvCustom Option
    { dropPrefixCount=3
    , customPrefix="RACKSPACE"
    }
