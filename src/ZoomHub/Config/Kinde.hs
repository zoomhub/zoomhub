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

import Crypto.JOSE (JWK)
import Data.Aeson (ToJSON, object, toJSON, (.=))
import qualified Data.Aeson as JSON
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Flow
import Network.URI (URI, parseRelativeReference, relativeTo)
import System.Environment (getEnvironment)
import ZoomHub.Types.BaseURI (BaseURI (..))

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
    logoutRedirectURI :: URI,
    jwk :: JWK
  }

fromEnv :: BaseURI -> IO (Maybe Config)
fromEnv baseURI = do
  env <- getEnvironment
  return $ do
    domain <- (env |> lookup "KINDE_DOMAIN") <&> T.pack .> Domain
    clientId <- (env |> lookup "KINDE_CLIENT_ID") <&> T.pack .> ClientId
    clientSecret <- (env |> lookup "KINDE_CLIENT_SECRET") <&> T.pack .> ClientSecret
    callbackPath <- parseRelativeReference "/auth/kinde/callback"
    let redirectURI = callbackPath `relativeTo` (unBaseURI baseURI)
    let logoutRedirectURI = unBaseURI baseURI
    jwk <- (env |> lookup "KINDE_JWK") <&> T.pack .> encodeUtf8 >>= JSON.decodeStrict
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
