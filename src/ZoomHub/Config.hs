{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Config
  ( Config(..)
  , ExistingContentStatus(..)
  , NewContentStatus(..)
  , RackspaceConfig
  , defaultPort
  , raxApiKey
  , raxContainer
  , raxContainerPath
  , raxUsername
  , toExistingContentStatus
  , toNewContentStatus
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
import ZoomHub.Storage.PostgreSQL2 (Connection)
import ZoomHub.Types.BaseURI (BaseURI)
import ZoomHub.Types.ContentBaseURI (ContentBaseURI)
import ZoomHub.Types.DatabasePath (DatabasePath)
import ZoomHub.Types.StaticBaseURI (StaticBaseURI)
import ZoomHub.Types.TempPath (TempPath)


defaultPort :: Integer
defaultPort = 8000

data Config = Config
  { baseURI                         :: BaseURI
  , contentBaseURI                  :: ContentBaseURI
  , dbConnInfo                      :: PGS.ConnectInfo
  , dbConnPool                      :: Pool Connection
  , dbConnPoolIdleTime              :: Second
  , dbConnPoolMaxResourcesPerStripe :: Integer
  , dbConnPoolNumStripes            :: Integer
  , dbPath                          :: DatabasePath
  , error404                        :: BL.ByteString
  , existingContentStatus           :: ExistingContentStatus
  , logger                          :: Middleware
  , newContentStatus                :: NewContentStatus
  , openSeadragonScript             :: String
  , port                            :: Integer
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
    , "dbPath" .= dbPath
    , "existingContentStatus" .= existingContentStatus
    , "newContentStatus" .= newContentStatus
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
    , raxApiKey = ""
    , raxContainer =
        case parseContainer "cache-development" of
          Just container -> container
          _ -> error $ "ZoomHub.Config.RackspaceConfig.defConfig:" ++
                       " Failed to parse `raxContainer`."
    , raxContainerPath =
        case parseRelativeReference "content" of
          Just containerPath -> containerPath
          _ -> error $ "ZoomHub.Config.RackspaceConfig.defConfig:" ++
                       " Failed to parse `raxContainerPath`."
    }

instance FromEnv RackspaceConfig where
  fromEnv = gFromEnvCustom Option
    { dropPrefixCount=3
    , customPrefix="RACKSPACE"
    }

-- ExistingContentStatus
data ExistingContentStatus = ProcessExistingContent | IgnoreExistingContent
  deriving (Eq, Show)

toExistingContentStatus :: String -> ExistingContentStatus
toExistingContentStatus "1"    = ProcessExistingContent
toExistingContentStatus "true" = ProcessExistingContent
toExistingContentStatus _      = IgnoreExistingContent

instance ToJSON ExistingContentStatus where
  toJSON = String . T.pack . show

-- NewContentStatus
data NewContentStatus = NewContentAllowed | NewContentDisallowed
  deriving (Eq, Show)

toNewContentStatus :: String -> NewContentStatus
toNewContentStatus "1"    = NewContentAllowed
toNewContentStatus "true" = NewContentAllowed
toNewContentStatus _      = NewContentDisallowed

instance ToJSON NewContentStatus where
  toJSON = String . T.pack . show
