{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.Internal.ContentState
  ( ContentState(..)
  , fromString
  ) where

import           Database.SQLite.Simple           (SQLData (SQLText))
import           Database.SQLite.Simple.FromField (FromField, ResultError (ConversionFailed),
                                                   fromField, returnError)
import           Database.SQLite.Simple.Internal  (Field (Field))
import           Database.SQLite.Simple.Ok        (Ok (Ok))
import           Database.SQLite.Simple.ToField   (ToField, toField)

data ContentState = Initialized | Active | CompletedSuccess | CompletedFailure
  deriving (Eq, Show)

fromString :: String -> Maybe ContentState
fromString "initialized" = Just Initialized
fromString "active" = Just Active
fromString "completed:success" = Just CompletedSuccess
fromString "completed:failure" = Just CompletedFailure
fromString _ = Nothing

instance ToField ContentState where
  toField Initialized      = SQLText "initialized"
  toField Active           = SQLText "active"
  toField CompletedSuccess = SQLText "completed:success"
  toField CompletedFailure = SQLText "completed:failure"

instance FromField ContentState where
  fromField (Field (SQLText "initialized") _) = Ok Initialized
  fromField (Field (SQLText "active") _) = Ok Active
  fromField (Field (SQLText "completed:success") _) = Ok CompletedSuccess
  fromField (Field (SQLText "completed:failure") _) = Ok CompletedFailure
  fromField f = returnError ConversionFailed f "Invalid `ContentState`"
