{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Config
  ( Config (..),
    defaultPort,
    APIUser,
  )
where

import Data.Aeson (ToJSON, object, toJSON, (.=))
import qualified Data.ByteString.Lazy as BL
import Data.Time.Units (Second)
import Data.Time.Units.Instances ()
import qualified Database.PostgreSQL.Simple as PGS
import Database.PostgreSQL.Simple.Instances ()
import Network.URI.Instances ()
import Network.Wai (Middleware)
import Squeal.PostgreSQL.Pool (Pool)
import qualified ZoomHub.Config.AWS as AWS
import ZoomHub.Config.ProcessContent (ProcessContent (..))
import ZoomHub.Config.Uploads (Uploads (..))
import ZoomHub.Storage.PostgreSQL (Connection)
import ZoomHub.Types.APIUser (APIUser)
import ZoomHub.Types.BaseURI (BaseURI)
import ZoomHub.Types.ContentBaseURI (ContentBaseURI)
import ZoomHub.Types.Environment (Environment)
import ZoomHub.Types.StaticBaseURI (StaticBaseURI)
import ZoomHub.Types.TempPath (TempPath)

defaultPort :: Integer
defaultPort = 8000

data Config = Config
  { apiUser :: APIUser,
    aws :: AWS.Config,
    baseURI :: BaseURI,
    contentBaseURI :: ContentBaseURI,
    dbConnInfo :: PGS.ConnectInfo,
    dbConnPool :: Pool Connection,
    dbConnPoolIdleTime :: Second,
    dbConnPoolMaxResourcesPerStripe :: Integer,
    dbConnPoolNumStripes :: Integer,
    environment :: Environment,
    error404 :: BL.ByteString,
    logger :: Middleware,
    openSeadragonScript :: String,
    port :: Integer,
    processContent :: ProcessContent,
    publicPath :: FilePath,
    staticBaseURI :: StaticBaseURI,
    tempPath :: TempPath,
    uploads :: Uploads,
    version :: String
  }

instance ToJSON Config where
  toJSON Config {..} =
    object
      [ "baseURI" .= baseURI,
        "contentBaseURI" .= contentBaseURI,
        "dbConnInfo" .= dbConnInfo,
        "dbConnPoolIdleTime" .= dbConnPoolIdleTime,
        "dbConnPoolMaxResourcesPerStripe" .= dbConnPoolMaxResourcesPerStripe,
        "dbConnPoolNumStripes" .= dbConnPoolNumStripes,
        "port" .= port,
        "processContent" .= processContent,
        "publicPath" .= publicPath,
        "staticBaseURI" .= staticBaseURI,
        "tempPath" .= tempPath,
        "uploads" .= uploads,
        "version" .= version
      ]
