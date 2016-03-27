{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.DeepZoomImage
  ( DeepZoomImage(DeepZoomImage)
  , TileFormat(JPEG, PNG)
  , TileOverlap(TileOverlap1, TileOverlap0)
  , dziWidth
  , dziHeight
  , dziTileSize
  , dziTileOverlap
  , dziTileFormat
  ) where


import           Data.Aeson                       (ToJSON,
                                                   Value (Number, String),
                                                   toJSON)
import qualified Data.Text                        as T
import           Database.SQLite.Simple           (SQLData (SQLText, SQLInteger))
import           Database.SQLite.Simple.FromField (FromField, ResultError (ConversionFailed),
                                                   fromField, returnError)
import           Database.SQLite.Simple.Internal  (Field (Field))
import           Database.SQLite.Simple.Ok        (Ok (Ok))
import           Database.SQLite.Simple.ToField   (ToField, toField)

data DeepZoomImage = DeepZoomImage
  { dziWidth       :: Integer
  , dziHeight      :: Integer
  , dziTileSize    :: Integer
  , dziTileOverlap :: TileOverlap
  , dziTileFormat  :: TileFormat
  } deriving (Eq, Show)

-- Tile overlap
data TileOverlap = TileOverlap1 | TileOverlap0 deriving (Eq, Show)

-- Tile overlap: JSON
instance ToJSON TileOverlap where
  toJSON TileOverlap1 = Number 1
  toJSON TileOverlap0 = Number 0

-- Tile overlap: SQLite
instance ToField TileOverlap where
  toField TileOverlap1 = SQLInteger 1
  toField TileOverlap0 = SQLInteger 0

instance FromField TileOverlap where
  fromField (Field (SQLInteger 1) _) = Ok TileOverlap1
  fromField (Field (SQLInteger 0) _) = Ok TileOverlap0
  fromField f = returnError ConversionFailed f "Invalid `TileOverlap`"

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
