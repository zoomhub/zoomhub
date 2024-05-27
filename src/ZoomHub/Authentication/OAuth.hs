{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Authentication.OAuth
  ( AuthorizationCode (..),
    State (..),
    Scope (..),
    AccessToken (..),
    RefreshToken (..),
    IdToken (..),
    generateState,
    AuthorizeState (..),
  )
where

import Crypto.Random (MonadRandom (getRandomBytes))
import Data.Aeson (FromJSON (parseJSON), ToJSON, Value (String))
import Data.Aeson.Types (typeMismatch)
import Data.Binary (Binary (get, put))
import qualified Data.ByteString.Base64.URL as URL
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Flow
import GHC.Generics (Generic)
import qualified Network.OAuth2.Experiment.Types as NOA2
import Servant (FromHttpApiData (parseQueryParam))

newtype AuthorizeState = AuthorizeState {unAuthorizeState :: NOA2.AuthorizeState}
  deriving (Eq)

instance Binary AuthorizeState where
  put (AuthorizeState s) = s |> NOA2.unAuthorizeState |> put
  get = AuthorizeState . NOA2.AuthorizeState <$> get

newtype AuthorizationCode = AuthorizationCode {unAuthorizationCode :: Text}
  deriving (FromHttpApiData)

newtype State = State {unState :: Text}
  deriving (FromHttpApiData)

newtype AccessToken = AccessToken {unAccessToken :: Text}
  deriving (FromJSON, ToJSON, Generic, Binary)

instance Show AccessToken where
  show _ = "<ZoomHub.Authentication.OAuth.AccessToken>"

newtype RefreshToken = RefreshToken {unRefreshToken :: Text}
  deriving (FromJSON, ToJSON, Generic, Binary)

instance Show RefreshToken where
  show _ = "<ZoomHub.Authentication.OAuth.RefreshToken>"

newtype IdToken = IdToken {unIdToken :: Text}
  deriving (FromJSON, ToJSON, Generic, Binary)

instance Show IdToken where
  show _ = "<ZoomHub.Authentication.OAuth.IdToken>"

scopeFromText :: Text -> Either Text Scope
scopeFromText t =
  let scope = t |> T.words |> Set.fromList
   in if scope == Set.empty
        then Left "Missing scope"
        else Right $ Scope scope

newtype Scope = Scope {unScope :: Set Text}
  deriving (Show)

instance FromHttpApiData Scope where
  parseQueryParam = scopeFromText

instance FromJSON Scope where
  parseJSON (String t) =
    case scopeFromText t of
      Right scope -> pure scope
      Left message -> fail $ "Invalid \"scope\":" <> T.unpack message
  parseJSON value = typeMismatch "String" value

generateState :: IO AuthorizeState
generateState = do
  randomBytes <- getRandomBytes 32
  return
    ( randomBytes
        |> URL.encodeBase64
        |> TL.fromStrict
        |> NOA2.AuthorizeState
        |> AuthorizeState
    )
