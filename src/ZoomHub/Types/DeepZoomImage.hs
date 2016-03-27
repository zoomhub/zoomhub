{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.DeepZoomImage
  ( DeepZoomImage(DeepZoomImage)
  , TileFormat(JPEG, PNG)
  , dziWidth
  , dziHeight
  , dziTileSize
  , dziTileOverlap
  , dziTileFormat
  ) where


import           Data.Aeson                       (ToJSON, Value (String),
                                                   toJSON)
import qualified Data.Text                        as T
import           Database.SQLite.Simple           (SQLData (SQLText))
import           Database.SQLite.Simple.FromField (FromField, ResultError (ConversionFailed),
                                                   fromField, returnError)
import           Database.SQLite.Simple.Internal  (Field (Field))
import           Database.SQLite.Simple.Ok        (Ok (Ok))
import           Database.SQLite.Simple.ToField   (ToField, toField)

data DeepZoomImage = DeepZoomImage
  { dziWidth       :: Integer
  , dziHeight      :: Integer
  , dziTileSize    :: Integer
  , dziTileOverlap :: Integer
  , dziTileFormat  :: TileFormat
  } deriving (Eq, Show)

-- Tile format
data TileFormat = JPEG | PNG deriving Eq

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
  fromField f = returnError ConversionFailed f "Invalid `TileFormat`"
