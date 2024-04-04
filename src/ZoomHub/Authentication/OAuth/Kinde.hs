{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Authentication.OAuth.Kinde
  ( mkApp,
    mkIdp,
    fetchAccessToken,
  )
where

import Control.Lens ((.~), (^?))
import qualified Data.Aeson as JSON
import Data.Aeson.Lens (key, _String)
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as L
import Flow
import Network.OAuth2.Experiment (AuthorizationCodeApplication (..), AuthorizeState, ClientId (ClientId), ClientSecret (ClientSecret), Idp (..), IdpApplication (IdpApplication))
import qualified Network.OAuth2.Experiment as OAuth2
import Network.Wreq (FormParam ((:=)), defaults, header, postWith, responseBody)
import URI.ByteString (parseURI, serializeURIRef', strictURIParserOptions)
import ZoomHub.Authentication.OAuth (AccessToken (AccessToken), AuthorizationCode (unAuthorizationCode))
import ZoomHub.Config.Kinde (ClientId (unClientId), ClientSecret (unClientSecret), Domain (unDomain))
import qualified ZoomHub.Config.Kinde as Kinde
import ZoomHub.Utils (lenientDecodeUtf8)

mkIdp :: Domain -> Idp "kinde"
mkIdp domain =
  Idp
    { idpAuthorizeEndpoint = uriFromText $ "https://" <> domain' <> ".us.kinde.com/oauth2/auth",
      idpTokenEndpoint = uriFromText $ "https://" <> domain' <> ".us.kinde.com/oauth2/token",
      idpUserInfoEndpoint = uriFromText $ "https://" <> domain' <> ".us.kinde.com/oauth2/v2/user_profile",
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

fetchAccessToken :: Idp "kinde" -> Kinde.Config -> AuthorizationCode -> IO (Maybe AccessToken)
fetchAccessToken idp config code = do
  let opts = defaults |> header "Content-Type" .~ ["application/x-www-form-urlencoded; charset=UTF-8"]
      tokenUrl = idp |> idpTokenEndpoint |> uriToText |> T.unpack
      payload =
        [ "client_id" := (config.clientId |> unClientId),
          "client_secret" := (config.clientSecret |> unClientSecret),
          "grant_type" := ("authorization_code" :: Text),
          "redirect_uri" := (config.redirectURI |> show),
          "code" := (code |> unAuthorizationCode)
        ]

  response <- postWith opts tokenUrl payload
  let accessToken = do
        body <- response ^? responseBody
        json <- (body |> JSON.decode) :: Maybe JSON.Value
        accessTokenStr <- json ^? key "access_token" . _String
        return $ AccessToken accessTokenStr
  return accessToken
  where
    uriToText uri = uri |> serializeURIRef' |> lenientDecodeUtf8
