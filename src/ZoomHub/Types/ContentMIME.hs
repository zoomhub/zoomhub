{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ZoomHub.Types.ContentMIME
  ( ContentMIME
  , ContentMIME'(ContentMIME)
  , unContentMIME
  , fromText
  ) where

import Codec.MIME.Parse (parseMIMEType)
import Codec.MIME.Type (Type, showType)
import Data.Aeson (ToJSON, Value(String), toJSON)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Database.SQLite.Simple (SQLData(SQLText))
import Database.SQLite.Simple.FromField
  (FromField, ResultError(ConversionFailed), fromField, returnError)
import Database.SQLite.Simple.Internal (Field(Field))
import Database.SQLite.Simple.Ok (Ok(Ok))
import Database.SQLite.Simple.ToField (ToField, toField)
import Squeal.PostgreSQL (FromValue(..), PG, PGType(PGtext), ToParam(..))


newtype ContentMIME' a = ContentMIME { unContentMIME :: a } deriving (Eq, Show)
type ContentMIME = ContentMIME' Type

toText :: ContentMIME -> T.Text
toText = showType . unContentMIME

fromText :: T.Text -> Maybe ContentMIME
fromText t = ContentMIME <$> parseMIMEType t


-- SQLite
instance ToField ContentMIME where
  toField = SQLText . toText

instance FromField ContentMIME where
  fromField f@(Field (SQLText t) _) =
    case parseMIMEType t of
      Just r  -> Ok (ContentMIME r)
      Nothing -> returnError ConversionFailed f
        ("invalid content MIME type: " ++ T.unpack t)
  fromField f = returnError ConversionFailed f "invalid MIME type"

-- JSON
instance ToJSON ContentMIME where
  toJSON = String . toText

-- Squeal / PostgreSQL
type instance PG ContentMIME = 'PGtext
instance ToParam ContentMIME 'PGtext where
  toParam = toParam . toText

instance FromValue 'PGtext ContentMIME where
  -- TODO: What if database value is not a valid MIME type?
  fromValue = ContentMIME . fromJust . parseMIMEType <$> fromValue @'PGtext
