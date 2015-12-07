{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Rackspace.CloudFiles where

import Control.Applicative as Applicative
import Control.Lens as Lens hiding ((.=)) -- .= used in Data.Aeson as well
import Data.Aeson as Aeson
import Data.Aeson.Lens as Aeson
import Network.Wreq as HTTP

import qualified Control.Monad.IO.Class as IO
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T


-- Types
data Credentials = Credentials
  { username :: String
  , apiKey :: String
  } deriving (Eq, Show)

instance Aeson.ToJSON Credentials where
    toJSON (Credentials username apiKey) =
      Aeson.object ["auth" .=
        Aeson.object ["RAX-KSKEY:apiKeyCredentials" .=
          Aeson.object ["username" .= username, "apiKey" .= apiKey]
        ]
      ]

newtype Endpoint = Endpoint String deriving (Eq, Show)
newtype Token = Token String deriving (Eq, Show)

-- API
tokenURL :: String
tokenURL = "https://identity.api.rackspacecloud.com/v2.0/tokens"

getResponse :: Credentials -> IO LBS.ByteString
getResponse credentials = do
  res <- HTTP.post tokenURL $ Aeson.toJSON credentials
  return $ res ^. responseBody

parseToken :: LBS.ByteString -> Maybe Token
parseToken res =
  let maybeToken = res ^? key "access" . key "token" . key "id" . _String in
  (Token . T.unpack) <$> maybeToken

parseEndpoint :: LBS.ByteString -> Maybe Endpoint
parseEndpoint res =
  -- TODO: How do I filter `access.serviceCatalog[].name == "IAD"` using
  -- lenses from:
  -- `{"access":{"serviceCatalog":[{"name":"IAD","endpoints":[]}]}}`
  -- let a = res ^? key "access" . key "serviceCatalog" . _Array in
  Just $ Endpoint "https://storage101.iad3.clouddrive.com/v1/MossoCloudFS_0c5dc6c2-028f-4648-a59d-e770b827add7"
