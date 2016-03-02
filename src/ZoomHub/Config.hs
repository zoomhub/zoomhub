{-# LANGUAGE DeriveGeneric #-}

module ZoomHub.Config
  ( Config(Config)
  , RackspaceConfig
  , acceptNewContent
  , baseURI
  , contentBaseURI
  , dataPath
  , defaultPort
  , encodeId
  , jobs
  , lastId
  , openseadragonScript
  , port
  , publicPath
  , rackspace
  , raxApiKey
  , raxUsername
  , version
  ) where

import           Control.Concurrent.STM       (TChan, TVar)
import           GHC.Generics                 (Generic)
import           System.Envy                  (DefConfig, FromEnv, Option (..),
                                               customPrefix, defConfig,
                                               dropPrefixCount, fromEnv,
                                               gFromEnvCustom)

import           ZoomHub.Types.BaseURI        (BaseURI)
import           ZoomHub.Types.ContentBaseURI (ContentBaseURI)

defaultPort :: Integer
defaultPort = 8000

data Config = Config
  { acceptNewContent    :: Bool
  , baseURI             :: BaseURI
  , contentBaseURI      :: ContentBaseURI
  , dataPath            :: FilePath
  , encodeId            :: Integer -> String
  , jobs                :: TChan String
  , lastId              :: TVar Integer
  , openseadragonScript :: String
  , port                :: Integer
  , publicPath          :: FilePath
  , rackspace           :: RackspaceConfig
  , version             :: String
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
