{-# LANGUAGE DeriveGeneric #-}

module ZoomHub.Types.DeepZoomImage
  ( DeepZoomImage(DeepZoomImage)
  , fromInternal
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Casing as AC
import qualified GHC.Generics as GHC
import qualified ZoomHub.Types.Internal.Content as IC
import qualified ZoomHub.Types.Internal.ContentId as IC
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
   toJSON = Aeson.genericToJSON $ AC.aesonPrefix AC.camelCase
instance Aeson.FromJSON DeepZoomImage where
   parseJSON = Aeson.genericParseJSON $ AC.aesonPrefix AC.camelCase

fromInternal :: IC.ContentId -> ID.DeepZoomImage -> DeepZoomImage
fromInternal cId dzi = DeepZoomImage
  -- TODO: Make hostname dynamic:
  { dziUrl = "http://content.zoomhub.net/dzis/" ++ (show cId) ++ ".dzi"
  , dziWidth = ID.dziWidth dzi
  , dziHeight = ID.dziHeight dzi
  , dziTileSize = ID.dziTileSize dzi
  , dziTileOverlap = ID.dziTileOverlap dzi
  , dziTileFormat = ID.dziTileFormat dzi
  }
