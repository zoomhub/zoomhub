{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Config
  ( Config (..),
    defaultPort,

    -- * Process content status
    ProcessContent (..),
    parseProcessContent,
  )
where

import Data.Aeson ((.=), ToJSON, Value (String), object, toJSON)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Time.Units (Second)
import qualified Database.PostgreSQL.Simple as PGS
import Database.PostgreSQL.Simple.Instances ()
import Network.URI.Instances ()
import Data.Time.Units.Instances ()
import Network.Wai (Middleware)
import Squeal.PostgreSQL.Pool (Pool)
import ZoomHub.Storage.PostgreSQL (Connection)
import ZoomHub.Types.BaseURI (BaseURI)
import ZoomHub.Types.ContentBaseURI (ContentBaseURI)
import ZoomHub.Types.StaticBaseURI (StaticBaseURI)
import ZoomHub.Types.TempPath (TempPath)
import qualified ZoomHub.Config.AWS as AWS

defaultPort :: Integer
defaultPort = 8000

data Config
  = Config
      { aws :: AWS.Config,
        baseURI :: BaseURI,
        contentBaseURI :: ContentBaseURI,
        dbConnInfo :: PGS.ConnectInfo,
        dbConnPool :: Pool Connection,
        dbConnPoolIdleTime :: Second,
        dbConnPoolMaxResourcesPerStripe :: Integer,
        dbConnPoolNumStripes :: Integer,
        error404 :: BL.ByteString,
        logger :: Middleware,
        openSeadragonScript :: String,
        port :: Integer,
        processContent :: ProcessContent,
        publicPath :: FilePath,
        staticBaseURI :: StaticBaseURI,
        tempPath :: TempPath,
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
        "processContent" .= processContent,
        "port" .= port,
        "publicPath" .= publicPath,
        "staticBaseURI" .= staticBaseURI,
        "tempPath" .= tempPath,
        "version" .= version
      ]

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
