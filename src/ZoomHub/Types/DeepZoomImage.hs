{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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


import           Data.Aeson                              (ToJSON, Value (Number, String),
                                                          genericToJSON, toJSON)
import           Data.Aeson.Casing                       (aesonPrefix,
                                                          camelCase)
import qualified Data.Text                               as T
import           Database.SQLite.Simple                  (SQLData (SQLText, SQLInteger))
import           Database.SQLite.Simple.FromField        (FromField, ResultError (ConversionFailed),
                                                          fromField,
                                                          returnError)
import           Database.SQLite.Simple.Internal         (Field (Field))
import           Database.SQLite.Simple.Ok               (Ok (Ok))
import           Database.SQLite.Simple.ToField          (ToField, toField)
import           GHC.Generics                            (Generic)
import           Text.Read                               (readMaybe)
import           Text.XML.Light                          (QName (QName))
import           Text.XML.Light.Input                    (parseXMLDoc)
import           Text.XML.Light.Proc                     (findAttr, findElement)

import           ZoomHub.Types.DeepZoomImage.TileOverlap (TileOverlap (..))
import qualified ZoomHub.Types.DeepZoomImage.TileOverlap as TileOverlap
import           ZoomHub.Types.DeepZoomImage.TileSize    (TileSize (..))
import qualified ZoomHub.Types.DeepZoomImage.TileSize    as TileSize

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
      \image -> attr "TileSize" image >>= TileSize.fromString >>=
      \tileSize -> attr "Overlap" image >>= TileOverlap.fromString >>=
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

-- Tile format
data TileFormat = JPEG | PNG deriving Eq

toTileFormat :: String -> Maybe TileFormat
toTileFormat "jpg" = Just JPEG
toTileFormat "jpeg" = Just JPEG
toTileFormat "png" = Just PNG
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
