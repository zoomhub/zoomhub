{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Config.Kinde
  ( Config (..),
    ClientId (unClientId),
    ClientSecret (unClientSecret),
    Domain (..),
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

newtype Domain = Domain {unDomain :: Text}
  deriving (ToJSON)

newtype ClientId = ClientId {unClientId :: Text}
  deriving (ToJSON)

newtype ClientSecret = ClientSecret {unClientSecret :: Text}
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
    pure Config {..}

instance ToJSON Config where
  toJSON (Config {..}) =
    object
      [ "domain" .= domain,
        "clientId" .= clientId,
        "clientSecret" .= ("<redacted>" :: Text),
        "redirectURI" .= (redirectURI |> show |> T.pack),
        "logoutRedirectURI" .= (logoutRedirectURI |> show |> T.pack)
      ]
