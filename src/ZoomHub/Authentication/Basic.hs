{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module ZoomHub.Authentication.Basic
  ( AuthenticatedUser,
    check,
  )
where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics (Generic)
import Servant (BasicAuthData (BasicAuthData))
import Servant.Auth.Server (AuthResult (Authenticated, NoSuchUser), BasicAuthCfg, FromBasicAuthData (fromBasicAuthData), FromJWT, ToJWT)
import ZoomHub.Types.APIUser (APIUser (APIUser))
import qualified ZoomHub.Types.APIUser as APIUser
import ZoomHub.Utils (lenientDecodeUtf8)

data AuthenticatedUser = AuthenticatedUser
  deriving (Show, Generic)

instance ToJSON AuthenticatedUser

instance FromJSON AuthenticatedUser

instance ToJWT AuthenticatedUser

instance FromJWT AuthenticatedUser

type instance BasicAuthCfg = BasicAuthData -> IO (AuthResult AuthenticatedUser)

check :: APIUser -> BasicAuthData -> IO (AuthResult AuthenticatedUser)
check apiUser (BasicAuthData unverifiedUsername unverifiedPassword) =
  let username = APIUser.username apiUser
      password = APIUser.password apiUser
   in if username == lenientDecodeUtf8 unverifiedUsername
        && password == lenientDecodeUtf8 unverifiedPassword
        then pure $ Authenticated AuthenticatedUser
        else pure NoSuchUser

instance FromBasicAuthData AuthenticatedUser where
  fromBasicAuthData authData checkFunction = checkFunction authData
