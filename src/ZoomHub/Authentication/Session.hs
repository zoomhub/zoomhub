{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Authentication.Session
  ( Session (..),
    User (..),
    DecodedIdToken (..),
    empty,
  )
where

import Crypto.JWT (ClaimsSet, HasClaimsSet (claimsSet))
import Data.Aeson (FromJSON, ToJSON (toJSON), Value (Object), object, parseJSON, withObject, (.:), (.:?), (.=))
import Data.Binary (Binary)
import Data.Text (Text)
import GHC.Generics (Generic)
import ZoomHub.Authentication.OAuth (AccessToken, RefreshToken)
import Prelude hiding (id)

data Session = Session
  { currentUser :: !(Maybe User),
    accessToken :: !(Maybe AccessToken),
    refreshToken :: !(Maybe RefreshToken)
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, Binary)

empty :: Session
empty =
  Session
    { currentUser = Nothing,
      accessToken = Nothing,
      refreshToken = Nothing
    }

data User = User
  { picture :: Maybe Text,
    familyName :: Maybe Text,
    givenName :: Maybe Text,
    email :: Text,
    id :: Maybe Text
  }
  deriving (Show, Generic)

instance Binary User

instance FromJSON User where
  parseJSON = withObject "User" $ \v -> do
    picture <- v .:? "picture"
    familyName <- v .:? "family_name"
    givenName <- v .:? "given_name"
    email <- v .: "email"
    id <- v .:? "id"
    return User {..}

instance ToJSON User where
  toJSON (User {..}) =
    object
      [ "picture" .= picture,
        "family_name" .= familyName,
        "given_name" .= givenName,
        "email" .= email,
        "id" .= id
      ]

data DecodedIdToken = DecodedIdToken {jwtClaims :: ClaimsSet, user :: User}
  deriving (Show)

instance HasClaimsSet DecodedIdToken where
  claimsSet f s = fmap (\a' -> s {jwtClaims = a'}) (f (jwtClaims s))

instance FromJSON DecodedIdToken where
  parseJSON = withObject "DecodedIdToken" $ \o -> do
    jwtClaims <- parseJSON (Object o)
    user <- parseJSON (Object o)
    return $ DecodedIdToken {jwtClaims, user}
