{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ZoomHub.Types.ContentState
  ( ContentState (..),
    fromString,
    isCompleted,
  )
where

import Data.Maybe (fromJust)
import Data.String (IsString)
import qualified Data.String as String
import Data.Text (Text)
import qualified Data.Text as T
import Squeal.PostgreSQL
  ( FromPG (fromPG),
    FromValue (..),
    Inline (..),
    IsPG,
    NP ((:*)),
    PG,
    PGType (PGenum, PGtext),
    ToPG (..),
    ToParam (..),
    enumValue,
    label,
  )

data ContentState
  = Initialized
  | Active
  | CompletedSuccess
  | CompletedFailure
  deriving (Eq, Show)

fromString :: String -> Maybe ContentState
fromString "initialized" = Just Initialized
fromString "active" = Just Active
fromString "completed:success" = Just CompletedSuccess
fromString "completed:failure" = Just CompletedFailure
fromString _ = Nothing

toText :: ContentState -> Text
toText Initialized = "initialized"
toText Active = "active"
toText CompletedSuccess = "completed:success"
toText CompletedFailure = "completed:failure"

isCompleted :: ContentState -> Bool
isCompleted state = case state of
  Initialized -> False
  Active -> False
  CompletedSuccess -> True
  CompletedFailure -> True

-- Squeal / PostgreSQL
instance IsPG ContentState where
  type PG ContentState = 'PGtext

instance FromPG ContentState where
  fromPG = fromJust . fromString <$> fromPG @String

instance ToPG db ContentState where
  toPG = toPG . toText

instance Inline ContentState where
  inline = inline . toText
