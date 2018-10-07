{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications      #-}

module ZoomHub.Types.DeepZoomImage.TileFormat
  ( TileFormat(..)
  , fromString
  , fromText
  ) where

import           Data.Aeson                       (ToJSON, Value (String),
                                                   toJSON)
import           Data.Maybe                       (fromJust)
import qualified Data.Text                        as T
import           Database.SQLite.Simple           (SQLData (SQLText))
import           Database.SQLite.Simple.FromField (FromField, ResultError (ConversionFailed),
                                                   fromField, returnError)
import           Database.SQLite.Simple.Internal  (Field (Field))
import           Database.SQLite.Simple.Ok        (Ok (Ok))
import           Database.SQLite.Simple.ToField   (ToField, toField)
import           Squeal.PostgreSQL                (FromValue (..),
                                                   PGType (PGtext))

data TileFormat = JPEG | PNG deriving Eq

fromString :: String -> Maybe TileFormat
fromString "jpg" = Just JPEG
fromString "jpeg" = Just JPEG
fromString "png" = Just PNG
fromString _ = Nothing

fromText :: T.Text -> Maybe TileFormat
fromText "jpg" = Just JPEG
fromText "jpeg" = Just JPEG
fromText "png" = Just PNG
fromText _ = Nothing

instance Show TileFormat where
  show JPEG = "jpg"
  show PNG = "png"

-- Tile format: JSON
instance ToJSON TileFormat where
  toJSON = String . T.pack . show

-- Tile format: SQLite
instance ToField TileFormat where
  toField = SQLText . T.pack . show

instance FromField TileFormat where
  fromField (Field (SQLText "jpg") _) = Ok JPEG
  fromField (Field (SQLText "png") _) = Ok PNG
  fromField f =
    returnError ConversionFailed f "invalid Deep Zoom image tile format"

-- Squeal / PostgreSQL
instance FromValue 'PGtext TileFormat where
  -- TODO: What if database value is not a valid?
  fromValue = fromJust . fromString <$> fromValue @'PGtext
