{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Authentication.OAuth.Kinde
  ( mkApp,
    mkIdp,
    fetchTokensFor,
    logoutURI,
    TokenCollection (..),
  )
where

import Control.Lens ((.~), (^?))
import qualified Data.Aeson as JSON (decode)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as L
import Flow
import Network.OAuth2.Experiment (AuthorizationCodeApplication (..), AuthorizeState, ClientId (ClientId), ClientSecret (ClientSecret), Idp (..), IdpApplication (IdpApplication))
import qualified Network.OAuth2.Experiment as OAuth2
import Network.Wreq (FormParam ((:=)), defaults, header, responseBody)
import qualified Network.Wreq as Wreq
import Servant (ToHttpApiData (toUrlPiece))
import URI.ByteString (URI, parseURI, strictURIParserOptions)
import URI.ByteString.Instances ()
import ZoomHub.Authentication.OAuth (AuthorizationCode (unAuthorizationCode))
import ZoomHub.Authentication.OAuth.Kinde.OAuth2CodeExchangeResponse (OAuth2CodeExchangeResponse)
import ZoomHub.Authentication.OAuth.Kinde.TokenCollection (TokenCollection (TokenCollection))
import ZoomHub.Config.Kinde (ClientId (unClientId), ClientSecret (unClientSecret), Domain (unDomain))
import qualified ZoomHub.Config.Kinde as Kinde
import ZoomHub.Utils (hush, tshow)

mkIdp :: Domain -> Idp "kinde"
mkIdp domain =
  Idp
    { idpAuthorizeEndpoint = uriFromText $ "https://" <> domain' <> "/oauth2/auth",
      idpTokenEndpoint = uriFromText $ "https://" <> domain' <> "/oauth2/token",
      idpUserInfoEndpoint = uriFromText $ "https://" <> domain' <> "/oauth2/v2/user_profile",
      idpDeviceAuthorizationEndpoint = Nothing
    }
  where
    domain' = domain |> unDomain |> encodeUtf8
    uriFromText uri =
      case parseURI strictURIParserOptions uri of
        Left uriParseError -> error $ "uriFromText: Invalid URI: " <> show uriParseError
        Right parsedURI -> parsedURI

mkAuthCodeApp :: Kinde.Config -> AuthorizeState -> AuthorizationCodeApplication
mkAuthCodeApp config state =
  AuthorizationCodeApplication
    { acClientId = config.clientId |> unClientId |> L.fromStrict |> ClientId,
      acClientSecret = config.clientSecret |> unClientSecret |> L.fromStrict |> ClientSecret,
      acScope = Set.fromList ["openid", "profile", "email", "offline"],
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

mkApp :: Kinde.Config -> AuthorizeState -> IdpApplication "kinde" AuthorizationCodeApplication
mkApp config state =
  IdpApplication
    { OAuth2.idp = mkIdp config.domain,
      OAuth2.application = mkAuthCodeApp config state
    }

logoutURI :: Kinde.Config -> Maybe URI
logoutURI config =
  parseURI
    strictURIParserOptions
    ( ( "https://"
          <> (config.domain |> unDomain)
          <> "/logout?redirect="
          <> (config.logoutRedirectURI |> tshow)
      )
        |> T.encodeUtf8
    )
    |> hush

fetchTokensFor ::
  Idp "kinde" ->
  Kinde.Config ->
  AuthorizationCode ->
  IO (Maybe OAuth2CodeExchangeResponse)
fetchTokensFor idp config authCode = do
  let opts =
        defaults
          |> header "Content-Type" .~ ["application/x-www-form-urlencoded; charset=UTF-8"]
          |> header "Kinde-SDK" .~ ["Haskell/0.0.1"]
      tokenUrl = idp |> idpTokenEndpoint |> toUrlPiece |> T.unpack
      payload =
        [ "client_id" := (config.clientId |> unClientId),
          "client_secret" := (config.clientSecret |> unClientSecret),
          "grant_type" := ("authorization_code" :: Text),
          "redirect_uri" := (config.redirectURI |> show),
          "code" := (authCode |> unAuthorizationCode)
        ]

  response <- Wreq.postWith opts tokenUrl payload
  -- TODO: Handle errors
  return $ response ^? responseBody >>= JSON.decode
