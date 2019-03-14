{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.ContentURI
  ( ContentURI
  , unContentURI
  ) where

import Data.Aeson (ToJSON, Value(String), toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Database.SQLite.Simple (SQLData(SQLText))
import Database.SQLite.Simple.FromField
  (FromField, ResultError(ConversionFailed), fromField, returnError)
import Database.SQLite.Simple.Internal (Field(Field))
import Database.SQLite.Simple.Ok (Ok(Ok))
import Database.SQLite.Simple.ToField (ToField, toField)
import Servant (FromHttpApiData, parseUrlPiece)

newtype ContentURI = ContentURI { unContentURI :: Text } deriving Eq

instance Show ContentURI where
  show = T.unpack . unContentURI

-- Text
instance FromHttpApiData ContentURI where
  parseUrlPiece t
    | "http://" `T.isPrefixOf` t             = Right $ ContentURI t
    | "https://" `T.isPrefixOf` t            = Right $ ContentURI t
    | "zoomit://thumbnail/" `T.isPrefixOf` t = Right $ ContentURI t
    | otherwise = Left "Invalid content URI"

-- JSON
instance ToJSON ContentURI where
  toJSON = String . unContentURI

-- SQLite
instance ToField ContentURI where
  toField = SQLText . unContentURI

instance FromField ContentURI where
  fromField field@(Field (SQLText t) _) =
    case parseUrlPiece t of
      Right r -> Ok r
      Left e  -> returnError ConversionFailed field (T.unpack e)
  fromField field = returnError ConversionFailed field "Invalid content URI"
