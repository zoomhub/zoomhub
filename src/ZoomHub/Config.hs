{-# LANGUAGE DeriveGeneric #-}

module ZoomHub.Config
  ( Config(Config)
  , RackspaceConfig
  , acceptNewContent
  , baseURI
  , contentBaseURI
  , dataPath
  , dbPath
  , dbConnection
  , defaultPort
  , encodeId
  , error404
  , logger
  , openseadragonScript
  , port
  , publicPath
  , rackspace
  , raxApiKey
  , raxUsername
  , version
  ) where

import qualified Data.ByteString.Lazy         as BL
import           Database.SQLite.Simple       (Connection)
import           GHC.Generics                 (Generic)
import           Network.Wai                  (Middleware)
import           System.Envy                  (DefConfig, FromEnv, Option (..),
                                               customPrefix, defConfig,
                                               dropPrefixCount, fromEnv,
                                               gFromEnvCustom)

import           ZoomHub.Types.BaseURI        (BaseURI)
import           ZoomHub.Types.ContentBaseURI (ContentBaseURI)
import           ZoomHub.Types.DatabasePath   (DatabasePath)


defaultPort :: Integer
defaultPort = 8000

data Config = Config
  { acceptNewContent    :: Bool
  , baseURI             :: BaseURI
  , contentBaseURI      :: ContentBaseURI
  , dataPath            :: FilePath
  , dbPath              :: DatabasePath
  , dbConnection        :: Connection
  , encodeId            :: Integer -> String
  , error404            :: BL.ByteString
  , logger              :: Middleware
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
