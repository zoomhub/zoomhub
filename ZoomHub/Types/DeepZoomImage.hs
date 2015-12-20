{-# LANGUAGE DeriveGeneric #-}

module ZoomHub.Types.DeepZoomImage where


import Data.Aeson as Aeson
import Data.Aeson.Casing

import qualified GHC.Generics as GHC
import qualified ZoomHub.Types.Internal.Content as IC
import qualified ZoomHub.Types.Internal.DeepZoomImage as ID


data DeepZoomImage = DeepZoomImage
  { dziUrl :: String
  , dziWidth :: Integer
  , dziHeight :: Integer
  , dziTileSize :: Integer
  , dziTileOverlap :: Integer
  , dziTileFormat :: String
  } deriving (Eq, Show, GHC.Generic)

instance Aeson.ToJSON DeepZoomImage where
   toJSON = genericToJSON $ aesonPrefix camelCase
instance Aeson.FromJSON DeepZoomImage where
   parseJSON = genericParseJSON $ aesonPrefix camelCase

fromInternal :: IC.ContentId -> ID.DeepZoomImage -> DeepZoomImage
fromInternal cid dzi = DeepZoomImage
  { dziUrl = "http://content.zoomhub.net/dzis/" ++ (show cid) ++ ".dzi"
  , dziWidth = ID.dziWidth dzi
  , dziHeight = ID.dziHeight dzi
  , dziTileSize = ID.dziTileSize dzi
  , dziTileOverlap = ID.dziTileOverlap dzi
  , dziTileFormat = ID.dziTileFormat dzi
  }
