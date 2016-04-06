{-# LANGUAGE DeriveGeneric #-}

module ZoomHub.Config
  ( Config(..)
  , ExistingContentStatus(..)
  , NewContentStatus(..)
  , defaultPort
  , toExistingContentStatus
  , toNewContentStatus
  ) where

import qualified Data.ByteString.Lazy         as BL
import           Data.Maybe                   (fromJust)
import           Database.SQLite.Simple       (Connection)
import           GHC.Generics                 (Generic)
import           Network.Wai                  (Middleware)
import           System.Envy                  (DefConfig, FromEnv, Option (..),
                                               customPrefix, defConfig,
                                               dropPrefixCount, fromEnv,
                                               gFromEnvCustom)

import           ZoomHub.Rackspace.CloudFiles (Container, parseContainer)
import           ZoomHub.Types.BaseURI        (BaseURI)
import           ZoomHub.Types.ContentBaseURI (ContentBaseURI)
import           ZoomHub.Types.DatabasePath   (DatabasePath)
import           ZoomHub.Types.StaticBaseURI  (StaticBaseURI)


defaultPort :: Integer
defaultPort = 8000

data Config = Config
  { baseURI               :: BaseURI
  , contentBaseURI        :: ContentBaseURI
  , staticBaseURI         :: StaticBaseURI
  , dataPath              :: FilePath
  , dbConnection          :: Connection
  , dbPath                :: DatabasePath
  , encodeId              :: Integer -> String
  , error404              :: BL.ByteString
  , existingContentStatus :: ExistingContentStatus
  , logger                :: Middleware
  , newContentStatus      :: NewContentStatus
  , openseadragonScript   :: String
  , port                  :: Integer
  , publicPath            :: FilePath
  , rackspace             :: RackspaceConfig
  , version               :: String
  }

-- Rackspace
data RackspaceConfig = RackspaceConfig
  { raxUsername  :: String    -- RACKSPACE_USERNAME
  , raxApiKey    :: String    -- RACKSPACE_API_KEY
  , raxContainer :: Container -- RACKSPACE_CONTAINER
  } deriving (Generic, Show)

-- Default configuration will be used for fields that could not be
-- retrieved from the environment:
instance DefConfig RackspaceConfig where
  defConfig = RackspaceConfig
    { raxUsername = "zoomingservice"
    , raxApiKey = ""
    , raxContainer = fromJust (parseContainer "cache-development")
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

-- NewContentStatus
data NewContentStatus = NewContentAllowed | NewContentDisallowed
  deriving (Eq, Show)

toNewContentStatus :: String -> NewContentStatus
toNewContentStatus "1"    = NewContentAllowed
toNewContentStatus "true" = NewContentAllowed
toNewContentStatus _      = NewContentDisallowed
