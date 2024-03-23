{-# LANGUAGE DeriveGeneric #-}

module ZoomHub.API.Types.DeepZoomImageWithoutURL
  ( DeepZoomImage (DeepZoomImage),
    dziHeight,
    dziTileFormat,
    dziTileOverlap,
    dziTileSize,
    dziWidth,
    mkDeepZoomImage,
    toInternal,
  )
where

import Data.Aeson (FromJSON, ToJSON, genericParseJSON, genericToJSON, parseJSON, toJSON)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Int (Int64)
import GHC.Generics (Generic)
import qualified ZoomHub.Types.DeepZoomImage as Internal

data DeepZoomImage = DeepZoomImage
  { dziWidth :: Int64,
    dziHeight :: Int64,
    dziTileSize :: Internal.TileSize,
    dziTileOverlap :: Internal.TileOverlap,
    dziTileFormat :: Internal.TileFormat
  }
  deriving (Eq, Show, Generic)

toInternal :: DeepZoomImage -> Internal.DeepZoomImage
toInternal dzi =
  Internal.mkDeepZoomImage
    (dziWidth dzi)
    (dziHeight dzi)
    (dziTileSize dzi)
    (dziTileOverlap dzi)
    (dziTileFormat dzi)

mkDeepZoomImage ::
  Int64 ->
  Int64 ->
  Internal.TileSize ->
  Internal.TileOverlap ->
  Internal.TileFormat ->
  DeepZoomImage
mkDeepZoomImage width height tileSize tileOverlap tileFormat =
  DeepZoomImage
    { dziWidth = width,
      dziHeight = height,
      dziTileSize = tileSize,
      dziTileOverlap = tileOverlap,
      dziTileFormat = tileFormat
    }

-- JSON
instance ToJSON DeepZoomImage where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON DeepZoomImage where
  parseJSON = genericParseJSON $ aesonPrefix camelCase
