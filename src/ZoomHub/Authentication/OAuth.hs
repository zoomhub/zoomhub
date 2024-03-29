{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Authentication.OAuth
  ( AuthorizationCode (..),
    State (..),
    Scopes (..),
    generateState,
  )
where

import Crypto.Random (MonadRandom (getRandomBytes))
import qualified Data.ByteString.Base64.URL as URL
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import Flow
import Servant (FromHttpApiData (parseQueryParam))

newtype AuthorizationCode = AuthorizationCode {unAuthorizationCode :: Text}
  deriving (FromHttpApiData)

newtype State = State {unState :: Text}
  deriving (FromHttpApiData)

newtype Scopes = Scopes {unScopes :: Set Text}

instance FromHttpApiData Scopes where
  parseQueryParam t =
    let scopes = t |> T.words |> Set.fromList
     in if scopes == Set.empty
          then Left "Missing scopes"
          else Right $ Scopes scopes

generateState :: IO Text
generateState = do
  randomBytes <- getRandomBytes 32
  return (randomBytes |> URL.encodeBase64)
