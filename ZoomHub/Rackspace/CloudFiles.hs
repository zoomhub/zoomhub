{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Rackspace.CloudFiles where

import Data.Aeson as Aeson

import qualified Control.Monad.IO.Class as IO
import qualified Data.ByteString.Lazy as LBS
import qualified Network.HTTP.Conduit as HTTP


data Credentials = Credentials
  { username :: String
  , apiKey :: String
  }

instance Aeson.ToJSON Credentials where
    toJSON (Credentials username apiKey) =
      Aeson.object ["auth" .=
        Aeson.object ["RAX-KSKEY:apiKeyCredentials" .=
          Aeson.object["username" .= username, "apiKey" .= apiKey]
        ]
      ]

tokenURL :: String
tokenURL = "https://identity.api.rackspacecloud.com/v2.0/tokens"

getJSON :: String -> -- ^ username
           String -> -- ^ API key
           IO LBS.ByteString
getJSON username apiKey = do
  req <- IO.liftIO $ HTTP.parseUrl tokenURL
  let credentials = Aeson.encode $ Credentials username apiKey
  let req' = req {
              HTTP.method = "POST"
            , HTTP.requestBody = HTTP.RequestBodyLBS credentials
            , HTTP.requestHeaders = [("Content-Type", "application/json")]
            }
  manager <- IO.liftIO $ HTTP.newManager HTTP.tlsManagerSettings
  res <- HTTP.httpLbs req' manager
  return $ HTTP.responseBody res
