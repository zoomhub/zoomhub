{-# LANGUAGE DeriveGeneric #-}

module ZoomHub.Types.DeepZoomImage
  ( DeepZoomImage(DeepZoomImage)
  , fromInternal
  ) where

import           Data.Aeson                           (FromJSON, ToJSON,
                                                       genericParseJSON,
                                                       genericToJSON, parseJSON,
                                                       toJSON)
import           Data.Aeson.Casing                    (aesonPrefix, camelCase)
import           GHC.Generics                         (Generic)

import           ZoomHub.Types.Internal.ContentId     (ContentId, unId)
import qualified ZoomHub.Types.Internal.DeepZoomImage as Internal


data DeepZoomImage = DeepZoomImage
  { dziUrl         :: String
  , dziWidth       :: Integer
  , dziHeight      :: Integer
  , dziTileSize    :: Integer
  , dziTileOverlap :: Integer
  , dziTileFormat  :: String
  } deriving (Eq, Show, Generic)

instance ToJSON DeepZoomImage where
   toJSON = genericToJSON $ aesonPrefix camelCase
instance FromJSON DeepZoomImage where
   parseJSON = genericParseJSON $ aesonPrefix camelCase

fromInternal :: ContentId -> Internal.DeepZoomImage -> DeepZoomImage
fromInternal cId dzi = DeepZoomImage
  -- TODO: Make hostname dynamic:
  { dziUrl = "http://content.zoomhub.net/dzis/" ++ unId cId ++ ".dzi"
  , dziWidth = Internal.dziWidth dzi
  , dziHeight = Internal.dziHeight dzi
  , dziTileSize = Internal.dziTileSize dzi
  , dziTileOverlap = Internal.dziTileOverlap dzi
  , dziTileFormat = Internal.dziTileFormat dzi
  }
