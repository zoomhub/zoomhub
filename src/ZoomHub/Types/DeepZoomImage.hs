{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Types.DeepZoomImage
  ( DeepZoomImage,
    TileFormat (..),
    TileOverlap (..),
    TileSize (..),
    dziWidth,
    dziHeight,
    dziTileSize,
    dziTileOverlap,
    dziTileFormat,
    mkDeepZoomImage,
    fromXML,
  )
where

import Data.Aeson (ToJSON, genericToJSON, toJSON)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import GHC.Generics (Generic)
import qualified Generics.SOP as SOP
import Text.Read (readMaybe)
import Text.XML.Light (QName (QName))
import Text.XML.Light.Input (parseXMLDoc)
import Text.XML.Light.Proc (findAttr, findElement)
import ZoomHub.Types.DeepZoomImage.TileFormat (TileFormat (..))
import qualified ZoomHub.Types.DeepZoomImage.TileFormat as TileFormat
import ZoomHub.Types.DeepZoomImage.TileOverlap (TileOverlap (..))
import qualified ZoomHub.Types.DeepZoomImage.TileOverlap as TileOverlap
import ZoomHub.Types.DeepZoomImage.TileSize (TileSize (..))
import qualified ZoomHub.Types.DeepZoomImage.TileSize as TileSize

data DeepZoomImage = DeepZoomImage
  { dziWidth :: Integer,
    dziHeight :: Integer,
    dziTileSize :: TileSize,
    dziTileOverlap :: TileOverlap,
    dziTileFormat :: TileFormat
  }
  deriving (Eq, Generic, Show)

mkDeepZoomImage ::
  Integer ->
  Integer ->
  TileSize ->
  TileOverlap ->
  TileFormat ->
  DeepZoomImage
mkDeepZoomImage dziWidth dziHeight dziTileSize dziTileOverlap dziTileFormat =
  DeepZoomImage {..}

fromXML :: String -> Maybe DeepZoomImage
fromXML xml = do
  xmlDoc <- parseXMLDoc xml
  image <- findElement (tag "Image") xmlDoc
  tileSize <- attr "TileSize" image >>= TileSize.fromString
  tileOverlap <- attr "Overlap" image >>= TileOverlap.fromString
  tileFormat <- attr "Format" image >>= TileFormat.fromString
  size <- findElement (tag "Size") image
  width <- attr "Width" size >>= readMaybe
  height <- attr "Height" size >>= readMaybe
  pure $ mkDeepZoomImage width height tileSize tileOverlap tileFormat
  where
    tag name = QName name (Just namespace) Nothing
    attr name = findAttr (QName name Nothing Nothing)
    namespace = "http://schemas.microsoft.com/deepzoom/2008"

-- JSON
instance ToJSON DeepZoomImage where
  toJSON = genericToJSON $ aesonPrefix camelCase

-- PostgreSQL / Squeal
instance SOP.Generic DeepZoomImage

instance SOP.HasDatatypeInfo DeepZoomImage
