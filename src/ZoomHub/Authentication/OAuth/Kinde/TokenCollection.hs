{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NoFieldSelectors #-}

module ZoomHub.Authentication.OAuth.Kinde.TokenCollection
  ( TokenCollection (..),
  )
where

import Data.Aeson (FromJSON, ToJSON (toJSON), object, parseJSON, withObject, (.:), (.=))
import Data.Binary (Binary)
import GHC.Generics (Generic)
import ZoomHub.Authentication.OAuth (AccessToken, IdToken, RefreshToken)
import Prelude hiding (id)

data TokenCollection = TokenCollection
  { accessToken :: !AccessToken,
    idToken :: !IdToken,
    refreshToken :: !RefreshToken
  }
  deriving (Show, Generic, Binary)

instance FromJSON TokenCollection where
  parseJSON = withObject "TokenCollection" $ \v -> do
    refreshToken <- v .: "refresh_token"
    accessToken <- v .: "access_token"
    idToken <- v .: "id_token"
    return TokenCollection {..}

instance ToJSON TokenCollection where
  toJSON (TokenCollection refresh access idT) =
    object
      [ "refresh_token" .= refresh,
        "access_token" .= access,
        "id_token" .= idT
      ]
