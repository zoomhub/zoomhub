{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Authentication.OAuth.Kinde
  ( idp,
    app,
  )
where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as L
import Flow
import Network.OAuth2.Experiment (AuthorizationCodeApplication (..), AuthorizeState, ClientId (ClientId), ClientSecret (ClientSecret), Idp (..), IdpApplication (IdpApplication))
import qualified Network.OAuth2.Experiment as OAuth2
import URI.ByteString (parseURI, strictURIParserOptions)
import ZoomHub.Config.Kinde (ClientId (unClientId), ClientSecret (unClientSecret), Domain (unDomain))
import qualified ZoomHub.Config.Kinde as Kinde

idp :: Domain -> Idp "kinde"
idp domain =
  Idp
    { idpAuthorizeEndpoint = uriFromText $ "https://" <> domain' <> ".us.kinde.com/oauth2/auth",
      idpTokenEndpoint = uriFromText $ "https://" <> domain' <> ".us.kinde.com/oauth2/token",
      idpUserInfoEndpoint = uriFromText $ "https://" <> domain' <> ".us.kinde.com/oauth2/v2/user_profile",
      idpDeviceAuthorizationEndpoint = Nothing
    }
  where
    domain' = unDomain domain |> encodeUtf8
    uriFromText uri =
      case parseURI strictURIParserOptions uri of
        Left uriParseError -> error $ "uriFromText: Invalid URI: " <> show uriParseError
        Right parsedURI -> parsedURI

authCodeApp :: Kinde.Config -> AuthorizeState -> AuthorizationCodeApplication
authCodeApp config state =
  AuthorizationCodeApplication
    { acClientId = config.clientId |> unClientId |> L.fromStrict |> ClientId,
      acClientSecret = config.clientSecret |> unClientSecret |> L.fromStrict |> ClientSecret,
      acScope = Set.fromList ["openid", "profile", "email"],
      acAuthorizeState = state,
      acRedirectUri = uriFromNetworkURI config.redirectURI,
      acName = "zoomhub",
      acAuthorizeRequestExtraParams = Map.empty,
      acTokenRequestAuthenticationMethod = OAuth2.ClientSecretBasic
    }
  where
    uriFromNetworkURI uri =
      case uri |> show |> BC.pack |> parseURI strictURIParserOptions of
        Left uriParseError -> error $ "fromURI: Invalid URI: " <> show uriParseError
        Right parsedURI -> parsedURI

app :: Kinde.Config -> AuthorizeState -> IdpApplication "kinde" AuthorizationCodeApplication
app config state =
  IdpApplication
    { OAuth2.idp = idp config.domain,
      OAuth2.application = authCodeApp config state
    }
