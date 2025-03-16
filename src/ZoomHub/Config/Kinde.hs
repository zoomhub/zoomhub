{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Config.Kinde
  ( Config (..),
    ClientId (..),
    ClientSecret (..),
    Domain (..),
    fromEnv,
  )
where

import Control.Lens ((^.))
import Control.Monad (guard)
import Crypto.JOSE (JWK, JWKSet (JWKSet))
import Data.Aeson (ToJSON, object, toJSON, (.=))
import qualified Data.Aeson as JSON
import Data.Functor ((<&>))
import Data.Text (Text)
import qualified Data.Text as T
import Flow
import Network.URI (URI, parseRelativeReference, relativeTo)
import Network.Wreq (responseBody)
import qualified Network.Wreq as Wreq
import Safe (headMay)
import System.Environment (getEnvironment)
import Text.Regex.PCRE ((=~))
import ZoomHub.Types.BaseURI (BaseURI (..))

newtype Domain = Domain {unDomain :: Text}
  deriving (ToJSON)

newtype ClientId = ClientId {unClientId :: Text}
  deriving (ToJSON)

newtype ClientSecret = ClientSecret {unClientSecret :: Text}
  deriving (ToJSON)

-- | Validates that the supplied client secret is in the correct format.
mkClientSecret :: Text -> Maybe ClientSecret
mkClientSecret secret = do
  let secretString = T.unpack secret
  guard $ secretString =~ ("^[a-zA-Z0-9]{40,60}$" :: String)
  pure $ ClientSecret secret

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
  let mDomain = (env |> lookup "KINDE_DOMAIN") <&> T.pack .> Domain
  mJWK <- case mDomain of
    Just domain -> fetchJWK domain
    Nothing -> pure Nothing
  return $ do
    domain <- mDomain
    clientId <- (env |> lookup "KINDE_CLIENT_ID") <&> T.pack .> ClientId
    clientSecret <- (env |> lookup "KINDE_CLIENT_SECRET") >>= mkClientSecret . T.pack
    callbackPath <- parseRelativeReference "/auth/kinde/callback"
    let redirectURI = callbackPath `relativeTo` unBaseURI baseURI
    let logoutRedirectURI = unBaseURI baseURI
    jwk <- mJWK
    pure Config {..}
  where
    fetchJWK :: Domain -> IO (Maybe JWK)
    fetchJWK (Domain domain) = do
      let url = "https://" <> T.unpack domain <> "/.well-known/jwks"
      response <- Wreq.get url
      let body = response ^. responseBody
          mKeySet = JSON.decode body :: Maybe JWKSet
      pure $ case mKeySet of
        Just (JWKSet keys) -> headMay keys
        Nothing -> Nothing

instance ToJSON Config where
  toJSON (Config {..}) =
    object
      [ "domain" .= domain,
        "clientId" .= clientId,
        "clientSecret" .= ("<redacted>" :: Text),
        "redirectURI" .= (redirectURI |> show |> T.pack),
        "logoutRedirectURI" .= (logoutRedirectURI |> show |> T.pack)
      ]
