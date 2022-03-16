{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.Environment
  ( Environment (..),
    fromEnv,
    fromText,
    toText,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Data.Aeson (ToJSON(toJSON), Value (String))

data Environment
  = Development
  | Test
  | Staging
  | Production

fromEnv :: IO (Maybe Environment)
fromEnv = do
  mValue <- lookupEnv "ZH_ENV"
  pure $ mValue >>= fromText . T.pack

fromText :: Text -> Maybe Environment
fromText "development" = Just Development
fromText "test" = Just Test
fromText "staging" = Just Staging
fromText "production" = Just Production
fromText _ = Nothing

toText :: Environment -> Text
toText = \case
  Development -> "development"
  Test -> "test"
  Staging -> "staging"
  Production -> "production"

-- JSON
instance ToJSON Environment where
  toJSON = String . toText
