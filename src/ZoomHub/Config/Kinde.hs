{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors #-}

module ZoomHub.Config.Kinde
  ( Config (..),
    fromEnv,
  )
where

import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as T
import Flow
import Network.URI (URI, parseAbsoluteURI)
import System.Environment (getEnvironment)

newtype Domain = Domain Text
  deriving (ToJSON)

newtype ClientId = ClientId Text
  deriving (ToJSON)

newtype ClientSecret = ClientSecret Text
  deriving (ToJSON)

data Config = Config
  { domain :: Domain,
    clientId :: ClientId,
    clientSecret :: ClientSecret,
    redirectURI :: URI,
    logoutRedirectURI :: URI
  }

fromEnv :: IO (Maybe Config)
fromEnv = do
  env <- getEnvironment
  return $ do
    domain <- (env |> lookup "KINDE_DOMAIN") <&> T.pack .> Domain
    clientId <- (env |> lookup "KINDE_CLIENT_ID") <&> T.pack .> ClientId
    clientSecret <- (env |> lookup "KINDE_CLIENT_SECRET") <&> T.pack .> ClientSecret
    redirectURI <- (env |> lookup "KINDE_REDIRECT_URI") >>= parseAbsoluteURI
    logoutRedirectURI <- (env |> lookup "KINDE_LOGOUT_REDIRECT_URI") >>= parseAbsoluteURI
    pure $
      Config
        { domain = domain,
          clientId = clientId,
          clientSecret = clientSecret,
          redirectURI = redirectURI,
          logoutRedirectURI = logoutRedirectURI
        }

instance ToJSON Config where
  toJSON (Config domain clientId _clientSecret redirectURI logoutRedirectURI) =
    object
      [ "domain" .= domain,
        "clientId" .= clientId,
        "clientSecret" .= ("<redacted>" :: Text),
        "redirectURI" .= (redirectURI |> show |> T.pack),
        "logoutRedirectURI" .= (logoutRedirectURI |> show |> T.pack)
      ]
