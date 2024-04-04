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
  )
where

import Amazonka.Data (FromJSON (parseJSON))
import Crypto.Random (MonadRandom (getRandomBytes))
import Data.Aeson (Value (String))
import Data.Aeson.Types (typeMismatch)
import qualified Data.ByteString.Base64.URL as URL
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import Flow
import Network.OAuth2.Experiment.Types (AuthorizeState (..))
import Servant (FromHttpApiData (parseQueryParam))

newtype AuthorizationCode = AuthorizationCode {unAuthorizationCode :: Text}
  deriving (FromHttpApiData)

newtype State = State {unState :: Text}
  deriving (FromHttpApiData)

newtype AccessToken = AccessToken {unAccessToken :: Text}
  deriving (FromJSON)

instance Show AccessToken where
  show _ = "<ZoomHub.Authentication.OAuth.AccessToken>"

newtype RefreshToken = RefreshToken {unRefreshToken :: Text}
  deriving (FromJSON)

instance Show RefreshToken where
  show _ = "<ZoomHub.Authentication.OAuth.RefreshToken>"

newtype IdToken = IdToken {unIdToken :: Text}
  deriving (FromJSON)

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
  return (randomBytes |> URL.encodeBase64 |> TL.fromStrict |> AuthorizeState)
