{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Authentication.OAuth.Kinde.OAuth2CodeExchangeResponse
  ( OAuth2CodeExchangeResponse (..),
    TokenType (..),
  )
where

import Data.Aeson (FromJSON (parseJSON), Value (String), withObject, (.:))
import ZoomHub.Authentication.OAuth (AccessToken (..), IdToken (..), RefreshToken (..), Scope (..))

data TokenType = Bearer
  deriving (Show)

instance FromJSON TokenType where
  parseJSON (String "bearer") = pure Bearer
  parseJSON _ = fail "Invalid \"token_type\""

data OAuth2CodeExchangeResponse = OAuth2CodeExchangeResponse
  { accessToken :: AccessToken,
    expiresIn :: Int,
    tokenType :: TokenType,
    refreshToken :: RefreshToken,
    idToken :: IdToken,
    scope :: Scope
  }
  deriving (Show)

instance FromJSON OAuth2CodeExchangeResponse where
  parseJSON = withObject "OAuth2CodeExchangeResponse" $ \obj -> do
    accessToken <- obj .: "access_token"
    expiresIn <- obj .: "expires_in"
    tokenType <- obj .: "token_type"
    refreshToken <- obj .: "refresh_token"
    idToken <- obj .: "id_token"
    scope <- obj .: "scope"
    return OAuth2CodeExchangeResponse {..}
