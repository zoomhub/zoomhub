{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ZoomHub.Types.DeepZoomImage
  ( DeepZoomImage
  , TileFormat(..)
  , TileOverlap(..)
  , TileSize(..)
  , dziWidth
  , dziHeight
  , dziTileSize
  , dziTileOverlap
  , dziTileFormat
  , mkDeepZoomImage
  , fromXML
  ) where


import           Data.Aeson                       (ToJSON,
                                                   Value (Number, String),
                                                   genericToJSON, toJSON)
import           Data.Aeson.Casing                (aesonPrefix, camelCase)
import qualified Data.Text                        as T
import           Database.SQLite.Simple           (SQLData (SQLText, SQLInteger))
import           Database.SQLite.Simple.FromField (FromField, ResultError (ConversionFailed),
                                                   fromField, returnError)
import           Database.SQLite.Simple.Internal  (Field (Field))
import           Database.SQLite.Simple.Ok        (Ok (Ok))
import           Database.SQLite.Simple.ToField   (ToField, toField)
import           GHC.Generics                     (Generic)
import           Text.Read                        (readMaybe)
import           Text.XML.Light                   (QName (QName))
import           Text.XML.Light.Input             (parseXMLDoc)
import           Text.XML.Light.Proc              (findAttr, findElement)

data DeepZoomImage = DeepZoomImage
  { dziWidth       :: Integer
  , dziHeight      :: Integer
  , dziTileSize    :: TileSize
  , dziTileOverlap :: TileOverlap
  , dziTileFormat  :: TileFormat
  } deriving (Eq, Generic, Show)

mkDeepZoomImage :: Integer ->
                   Integer ->
                   TileSize ->
                   TileOverlap ->
                   TileFormat ->
                   DeepZoomImage
mkDeepZoomImage dziWidth dziHeight dziTileSize dziTileOverlap dziTileFormat =
  DeepZoomImage{..}

fromXML :: String -> Maybe DeepZoomImage
fromXML xml =
      parseXMLDoc xml >>=
      findElement (tag "Image") >>=
      \image -> attr "TileSize" image >>= toTileSize >>=
      \tileSize -> attr "Overlap" image >>= toTileOverlap >>=
      \tileOverlap -> attr "Format" image >>= toTileFormat >>=
      \tileFormat -> findElement (tag "Size") image >>=
      \size -> attr "Width" size >>= readMaybe >>=
      \width -> attr "Height" size >>= readMaybe >>=
      \height ->
        Just $ mkDeepZoomImage width height tileSize tileOverlap tileFormat
  where
    tag name = QName name (Just namespace) Nothing
    attr name = findAttr (QName name Nothing Nothing)
    namespace = "http://schemas.microsoft.com/deepzoom/2008"

-- JSON
instance ToJSON DeepZoomImage where
   toJSON = genericToJSON $ aesonPrefix camelCase

-- Tile size
data TileSize = TileSize254 | TileSize256 | TileSize1024
  deriving (Bounded, Enum, Eq)

toTileSize :: String -> Maybe TileSize
toTileSize "254" = Just TileSize254
toTileSize "256" = Just TileSize256
toTileSize "1024" = Just TileSize1024
toTileSize _ = Nothing

instance Show TileSize where
  show TileSize254 = "254"
  show TileSize256 = "256"
  show TileSize1024 = "1024"

-- Tile size: JSON
instance ToJSON TileSize where
  toJSON TileSize254 = Number 254
  toJSON TileSize256 = Number 256
  toJSON TileSize1024 = Number 1024

-- Tile size: SQLite
instance ToField TileSize where
  toField TileSize254 = SQLInteger 254
  toField TileSize256 = SQLInteger 256
  toField TileSize1024 = SQLInteger 1024

instance FromField TileSize where
  fromField (Field (SQLInteger 254) _) = Ok TileSize254
  fromField (Field (SQLInteger 256) _) = Ok TileSize256
  fromField (Field (SQLInteger 1024) _) = Ok TileSize1024
  fromField f =
    returnError ConversionFailed f "invalid Deep Zoom image tile size"

-- Tile overlap
data TileOverlap = TileOverlap0 | TileOverlap1
  deriving (Bounded, Enum, Eq)

toTileOverlap :: String -> Maybe TileOverlap
toTileOverlap "0" = Just TileOverlap0
toTileOverlap "1" = Just TileOverlap1
toTileOverlap _ = Nothing

-- Tile overlap: Show
instance Show TileOverlap where
  show TileOverlap0 = "0"
  show TileOverlap1 = "1"

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
  fromField f =
    returnError ConversionFailed f "invalid Deep Zoom image tile overlap"

-- Tile format
data TileFormat = JPEG | PNG deriving Eq

toTileFormat :: String -> Maybe TileFormat
toTileFormat "jpg" = Just JPEG
toTileFormat "jpeg" = Just JPEG
toTileFormat "png" = Just JPEG
toTileFormat _ = Nothing

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
