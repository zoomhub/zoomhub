{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Config
  ( Config(..)
  , RackspaceConfig
  , defaultPort
  , raxApiKey
  , raxContainer
  , raxContainerPath
  , raxUsername
    -- * Process content status
  , ProcessContent(..)
  , parseProcessContent
  ) where

import Data.Aeson (ToJSON, Value(String), object, toJSON, (.=))
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Time.Units (Second)
import qualified Database.PostgreSQL.Simple as PGS
import Database.PostgreSQL.Simple.Instances ()
import GHC.Generics (Generic)
import Network.URI (URI, parseRelativeReference)
import Network.URI.Instances ()
import Network.Wai (Middleware)
import Squeal.PostgreSQL.Pool (Pool)
import System.Envy
  ( DefConfig
  , FromEnv
  , Option(..)
  , customPrefix
  , defConfig
  , dropPrefixCount
  , fromEnv
  , gFromEnvCustom
  )

import ZoomHub.Rackspace.CloudFiles (Container, parseContainer)
import ZoomHub.Storage.PostgreSQL (Connection)
import ZoomHub.Types.BaseURI (BaseURI)
import ZoomHub.Types.ContentBaseURI (ContentBaseURI)
import ZoomHub.Types.StaticBaseURI (StaticBaseURI)
import ZoomHub.Types.TempPath (TempPath)


defaultPort :: Integer
defaultPort = 5000

data Config = Config
  { baseURI                         :: BaseURI
  , contentBaseURI                  :: ContentBaseURI
  , dbConnInfo                      :: PGS.ConnectInfo
  , dbConnPool                      :: Pool Connection
  , dbConnPoolIdleTime              :: Second
  , dbConnPoolMaxResourcesPerStripe :: Integer
  , dbConnPoolNumStripes            :: Integer
  , error404                        :: BL.ByteString
  , logger                          :: Middleware
  , openSeadragonScript             :: String
  , port                            :: Integer
  , processContent                  :: ProcessContent
  , publicPath                      :: FilePath
  , rackspace                       :: RackspaceConfig
  , staticBaseURI                   :: StaticBaseURI
  , tempPath                        :: TempPath
  , version                         :: String
  }

instance ToJSON Config where
  toJSON Config{..} = object
    [ "baseURI" .= baseURI
    , "contentBaseURI" .= contentBaseURI
    , "dbConnInfo" .= dbConnInfo
    , "dbConnPoolIdleTime" .= dbConnPoolIdleTime
    , "dbConnPoolMaxResourcesPerStripe" .= dbConnPoolMaxResourcesPerStripe
    , "dbConnPoolNumStripes" .= dbConnPoolNumStripes
    , "processContent" .= processContent
    , "port" .= port
    , "publicPath" .= publicPath
    , "staticBaseURI" .= staticBaseURI
    , "tempPath" .= tempPath
    , "version" .= version
    ]

-- Rackspace
data RackspaceConfig = RackspaceConfig
  { raxUsername      :: String    -- RACKSPACE_USERNAME
  , raxApiKey        :: String    -- RACKSPACE_API_KEY
  , raxContainer     :: Container -- RACKSPACE_CONTAINER
  , raxContainerPath :: URI       -- RACKSPACE_CONTAINER_PATH
  } deriving (Generic, Show)

-- Default configuration will be used for fields that could not be
-- retrieved from the environment:
instance DefConfig RackspaceConfig where
  defConfig = RackspaceConfig
    { raxUsername = "zoomingservice"
    , raxApiKey = error "Missing `raxApiKey`"
    , raxContainer =
        case parseContainer "cache-development" of
          Just container -> container
          _ -> error "Failed to parse `raxContainer`."
    , raxContainerPath =
        case parseRelativeReference "content" of
          Just containerPath -> containerPath
          _ -> error "Failed to parse `raxContainerPath`."
    }

instance FromEnv RackspaceConfig where
  fromEnv = gFromEnvCustom Option
    { dropPrefixCount=3
    , customPrefix="RACKSPACE"
    }

-- ProcessContent
data ProcessContent
  = ProcessNoContent
  | ProcessExistingContent
  | ProcessExistingAndNewContent
  deriving (Eq, Show)

parseProcessContent :: String -> ProcessContent
parseProcessContent "ProcessExistingContent" = ProcessExistingContent
parseProcessContent "ProcessExistingAndNewContent" = ProcessExistingAndNewContent
parseProcessContent _ = ProcessNoContent

instance ToJSON ProcessContent where
  toJSON = String . T.pack . show
