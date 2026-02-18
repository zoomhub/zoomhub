{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Authentication.Session
  ( Session (..),
    KindeUser (..),
    DecodedIdToken (..),
    isTokenExpired,
    tokenExpiresAtFromExpiresIn,
  )
where

import Crypto.JWT (ClaimsSet, HasClaimsSet (claimsSet))
import Data.Aeson (FromJSON, ToJSON (toJSON), Value (Object), object, parseJSON, withObject, (.:), (.:?), (.=))
import Data.Binary (Binary)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics (Generic)
import ZoomHub.Authentication.OAuth (AccessToken, RefreshToken)
import Prelude hiding (id)

data Session = Session
  { kindeUser :: !KindeUser,
    accessToken :: !AccessToken,
    refreshToken :: !RefreshToken,
    tokenExpiresAt :: !Int64
  }
  deriving stock (Generic)
  deriving anyclass (FromJSON, ToJSON, Binary)

-- | Check if the access token has expired (with 5-minute buffer)
isTokenExpired :: Session -> IO Bool
isTokenExpired session = do
  now <- getPOSIXTime
  let bufferSeconds = 5 * 60
  return $ round now + bufferSeconds >= session.tokenExpiresAt

-- | Compute token expiry timestamp from `expires_in` seconds
tokenExpiresAtFromExpiresIn :: Int -> IO Int64
tokenExpiresAtFromExpiresIn expiresIn = do
  now <- getPOSIXTime
  return $ round now + fromIntegral expiresIn

--
data KindeUser = KindeUser
  { id :: !Text,
    email :: !Text,
    isEmailVerified :: !Bool,
    givenName :: !(Maybe Text),
    familyName :: !(Maybe Text),
    picture :: !(Maybe Text)
  }
  deriving (Show, Generic)

instance Binary KindeUser

instance FromJSON KindeUser where
  parseJSON = withObject "User" $ \v -> do
    picture <- v .:? "picture"
    familyName <- v .:? "family_name"
    givenName <- v .:? "given_name"
    email <- v .: "email"
    isEmailVerified <- v .: "email_verified"
    id <- v .: "sub"
    return KindeUser {..}

-- For debugging only
instance ToJSON KindeUser where
  toJSON (KindeUser {..}) =
    object
      [ "picture" .= picture,
        "family_name" .= familyName,
        "given_name" .= givenName,
        "email" .= email,
        "email_verified" .= isEmailVerified,
        "sub" .= id
      ]

data DecodedIdToken = DecodedIdToken
  { jwtClaims :: ClaimsSet,
    user :: KindeUser
  }
  deriving (Show)

instance HasClaimsSet DecodedIdToken where
  claimsSet f s = fmap (\a' -> s {jwtClaims = a'}) (f (jwtClaims s))

instance FromJSON DecodedIdToken where
  parseJSON = withObject "DecodedIdToken" $ \o -> do
    jwtClaims <- parseJSON (Object o)
    user <- parseJSON (Object o)
    return $ DecodedIdToken {jwtClaims, user}
