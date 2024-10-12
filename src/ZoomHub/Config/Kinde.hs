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

import Control.Monad (guard)
import Crypto.JOSE (JWK)
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Flow
import Network.URI (URI, parseRelativeReference, relativeTo)
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import System.Environment (getEnvironment)
import Text.Regex.PCRE ((=~))
import ZoomHub.Types.BaseURI (BaseURI(..))

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
  return $ do
    domain <- (env |> lookup "KINDE_DOMAIN") <&> T.pack .> Domain
    clientId <- (env |> lookup "KINDE_CLIENT_ID") <&> T.pack .> ClientId
    clientSecret <- (env |> lookup "KINDE_CLIENT_SECRET") >>= mkClientSecret . T.pack
    callbackPath <- parseRelativeReference "/auth/kinde/callback"
    let redirectURI = callbackPath `relativeTo` unBaseURI baseURI
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
