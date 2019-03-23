{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ZoomHub.Types.ContentState
  ( ContentState(..)
  , ContentStateColumn
  , fromString
  , toColumn
    -- Squeal / Postgres
  , defaultValue
  , toText
  ) where

import Data.Maybe (fromJust)
import Data.Profunctor.Product.Default (Default, def)
import Data.Text (Text)
import qualified Database.PostgreSQL.Simple.FromField as PGS
import Database.SQLite.Simple (SQLData(SQLText))
import Database.SQLite.Simple.FromField
  (FromField, ResultError(ConversionFailed), fromField, returnError)
import Database.SQLite.Simple.Internal (Field(Field))
import Database.SQLite.Simple.Ok (Ok(Ok))
import Database.SQLite.Simple.ToField (ToField, toField)
import Opaleye
  ( Column
  , Constant(Constant)
  , PGText
  , QueryRunnerColumnDefault
  , fieldQueryRunnerColumn
  , pgStrictText
  , queryRunnerColumnDefault
  )
import Squeal.PostgreSQL (FromValue(..), PG, PGType(PGtext), ToParam(..))

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

-- SQLite
instance ToField ContentState where
  toField = SQLText . toText

instance FromField ContentState where
  fromField (Field (SQLText "initialized") _) = Ok Initialized
  fromField (Field (SQLText "active") _) = Ok Active
  fromField (Field (SQLText "completed:success") _) = Ok CompletedSuccess
  fromField (Field (SQLText "completed:failure") _) = Ok CompletedFailure
  fromField f = returnError ConversionFailed f "invalid content state"

-- PostgreSQL
type ContentStateColumn = Column PGText

toColumn :: ContentState -> ContentStateColumn
toColumn = pgStrictText . toText

instance PGS.FromField ContentState where
  fromField f mdata = PGS.fromField f mdata >>= parseContentState
    where
      parseContentState :: Maybe Text -> PGS.Conversion ContentState
      parseContentState r = case r of
        Just "initialized"       -> return Initialized
        Just "active"            -> return Active
        Just "completed:success" -> return CompletedSuccess
        Just "completed:failure" -> return CompletedFailure
        Just _  -> PGS.returnError PGS.ConversionFailed f "unrecognized content state"
        Nothing -> PGS.returnError PGS.UnexpectedNull f "empty content state"

instance QueryRunnerColumnDefault PGText ContentState where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance Default Constant ContentState ContentStateColumn where
  def = Constant toColumn

-- Squeal / PostgreSQL
defaultValue :: ContentState
defaultValue = Initialized

instance FromValue 'PGtext ContentState where
  -- TODO: What if database value is not a valid?
  fromValue = fromJust . fromString <$> fromValue @'PGtext

type instance PG ContentState = 'PGtext
instance ToParam ContentState 'PGtext where
  toParam = toParam . toText
