{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.ContentURI
  ( ContentURI
  , unContentURI
  ) where

import           Data.Aeson                       (ToJSON, Value (String),
                                                   toJSON)
import           Data.Text                        (Text)
import qualified Data.Text                        as T
import           Database.SQLite.Simple           (SQLData (SQLText))
import           Database.SQLite.Simple.FromField (FromField, ResultError (ConversionFailed),
                                                   fromField, returnError)
import           Database.SQLite.Simple.Internal  (Field (Field))
import           Database.SQLite.Simple.Ok        (Ok (Ok))
import           Database.SQLite.Simple.ToField   (ToField, toField)
import           Servant                          (FromText, fromText)


-- TODO: Use record syntax, i.e. `ContentURI { unContentURI :: URI }` without
-- introducing `{"contentURI": <contentURI>}` JSON serialization:
newtype ContentURI = ContentURI Text deriving Eq

unContentURI :: ContentURI -> Text
unContentURI (ContentURI uri) = uri

instance Show ContentURI where
  show = T.unpack . unContentURI

-- Text
instance FromText ContentURI where
  fromText t
    | "http://"  `T.isPrefixOf` t = Just (ContentURI t)
    | "https://" `T.isPrefixOf` t = Just (ContentURI t)
    | otherwise = Nothing

-- JSON
instance ToJSON ContentURI where
  toJSON = String . unContentURI

-- SQLite
instance ToField ContentURI where
  toField = SQLText . unContentURI

instance FromField ContentURI where
  fromField field@(Field (SQLText t) _) =
    case fromText t of
      Just r  -> Ok r
      Nothing -> returnError ConversionFailed field "Invalid `ContentURI`"
  fromField field = returnError ConversionFailed field "Invalid `ContentURI`"
