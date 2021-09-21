{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.Environment
  ( Environment (..),
    fromEnv,
    fromText,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)

data Environment
  = Development
  | Test
  | Production

fromEnv :: IO (Maybe Environment)
fromEnv = do
  mValue <- lookupEnv "ZH_ENV"
  pure $ mValue >>= fromText . T.pack

fromText :: Text -> Maybe Environment
fromText "development" = Just Development
fromText "test" = Just Test
fromText "production" = Just Production
fromText _ = Nothing
