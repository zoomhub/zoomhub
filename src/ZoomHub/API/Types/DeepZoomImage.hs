{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.API.Types.DeepZoomImage
  ( DeepZoomImage (DeepZoomImage),
    DeepZoomImageURI (DeepZoomImageURI),
    dziHeight,
    dziTileFormat,
    dziTileOverlap,
    dziTileSize,
    dziUrl,
    dziWidth,
    fromInternal,
    mkDeepZoomImage,
  )
where

import Data.Aeson (ToJSON, Value (String), genericToJSON, toJSON)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Generics.SOP as SOP
import Network.URI (URI, parseRelativeReference, relativeTo)
import System.FilePath ((<.>), (</>))
import ZoomHub.Types.ContentBaseURI
  ( ContentBaseURI, unContentBaseURI
  )
import ZoomHub.Types.ContentId (ContentId, unContentId)
import qualified ZoomHub.Types.DeepZoomImage as Internal

data DeepZoomImage
  = DeepZoomImage
      { dziUrl :: DeepZoomImageURI,
        dziWidth :: Integer,
        dziHeight :: Integer,
        dziTileSize :: Internal.TileSize,
        dziTileOverlap :: Internal.TileOverlap,
        dziTileFormat :: Internal.TileFormat
      }
  deriving (Eq, Show, Generic)

fromInternal ::
  ContentBaseURI ->
  ContentId ->
  Internal.DeepZoomImage ->
  Maybe DeepZoomImage
fromInternal baseURI cId dzi = do
  namePath <- parseRelativeReference name
  let url = namePath `relativeTo` unContentBaseURI baseURI
  pure $ DeepZoomImage { dziUrl = DeepZoomImageURI url,
    dziWidth = Internal.dziWidth dzi,
    dziHeight = Internal.dziHeight dzi,
    dziTileSize = Internal.dziTileSize dzi,
    dziTileOverlap = Internal.dziTileOverlap dzi,
    dziTileFormat = Internal.dziTileFormat dzi
  }
  where
    name = unContentId cId <.> "dzi"

mkDeepZoomImage ::
  DeepZoomImageURI ->
  Integer ->
  Integer ->
  Internal.TileSize ->
  Internal.TileOverlap ->
  Internal.TileFormat ->
  DeepZoomImage
mkDeepZoomImage uri width height tileSize tileOverlap tileFormat = DeepZoomImage
  { dziUrl = uri,
    dziWidth = width,
    dziHeight = height,
    dziTileSize = tileSize,
    dziTileOverlap = tileOverlap,
    dziTileFormat = tileFormat
  }

-- JSON
instance ToJSON DeepZoomImage where
  toJSON = genericToJSON $ aesonPrefix camelCase

-- Types
newtype DeepZoomImageURI = DeepZoomImageURI {unDeepZoomImageURI :: URI}
  deriving (Eq)

instance Show DeepZoomImageURI where
  show = show . unDeepZoomImageURI

instance ToJSON DeepZoomImageURI where
  toJSON = String . T.pack . show . unDeepZoomImageURI

-- PostgreSQL / Squeal
instance SOP.Generic DeepZoomImage

instance SOP.HasDatatypeInfo DeepZoomImage
