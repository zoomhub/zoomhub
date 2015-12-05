{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Rackspace.CloudFiles where

import Control.Applicative as Applicative
import Data.Aeson as Aeson

import qualified Control.Monad.IO.Class as IO
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Conduit as HTTP


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

getToken :: Credentials -> IO (Maybe Token)
getToken credentials = do
  req <- IO.liftIO $ HTTP.parseUrl tokenURL
  let credentialsBS = Aeson.encode $ credentials
  let req' = req {
              HTTP.method = "POST"
            , HTTP.requestBody = HTTP.RequestBodyLBS credentialsBS
            , HTTP.requestHeaders = [("Content-Type", "application/json")]
            }
  manager <- IO.liftIO $ HTTP.newManager HTTP.tlsManagerSettings
  res <- HTTP.httpLbs req' manager
  let maybeToken = decode $ HTTP.responseBody res
  return maybeToken
