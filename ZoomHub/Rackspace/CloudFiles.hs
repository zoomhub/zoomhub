{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Rackspace.CloudFiles where

import Control.Applicative as Applicative
import Control.Lens as Lens hiding ((.=)) -- .= used in Data.Aeson as well
import Data.Aeson as Aeson
import Network.Wreq as HTTP

import qualified Control.Monad.IO.Class as IO
import qualified Data.ByteString.Lazy as LBS


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

data Token = Token String deriving (Eq, Show)

instance Aeson.FromJSON Token where
    parseJSON (Object o) =
      Token <$> ((o .: "access") >>= (.: "token") >>= (.: "id"))
    parseJSON _ = empty


-- API
tokenURL :: String
tokenURL = "https://identity.api.rackspacecloud.com/v2.0/tokens"

getJSON :: Credentials -> IO LBS.ByteString
getJSON credentials = do
  res <- HTTP.post tokenURL $ Aeson.toJSON credentials
  return $ res ^. responseBody

getToken :: Credentials  -> IO (Maybe Token)
getToken credentials = do
  res <- getJSON credentials
  let maybeToken = decode $ res
  return maybeToken
