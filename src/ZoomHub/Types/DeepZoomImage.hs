{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.DeepZoomImage
  ( DeepZoomImage(DeepZoomImage)
  , fromInternal
  , mkDeepZoomImage
  ) where

import           Data.Aeson                           (FromJSON, ToJSON,
                                                       genericParseJSON,
                                                       genericToJSON, parseJSON,
                                                       toJSON)
import           Data.Aeson.Casing                    (aesonPrefix, camelCase)
import           GHC.Generics                         (Generic)

import           ZoomHub.Types.Internal.ContentId     (ContentId, unId)
import qualified ZoomHub.Types.Internal.DeepZoomImage as Internal

type URL = String

data DeepZoomImage = DeepZoomImage
  { dziUrl         :: String
  , dziWidth       :: Integer
  , dziHeight      :: Integer
  , dziTileSize    :: Integer
  , dziTileOverlap :: Integer
  , dziTileFormat  :: String
  } deriving (Eq, Show, Generic)

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

mkDeepZoomImage :: URL ->
                   Integer ->
                   Integer ->
                   Integer ->
                   Integer ->
                   String ->
                   DeepZoomImage
mkDeepZoomImage url width height tileSize tileOverlap tileFormat = DeepZoomImage
  { dziUrl = url
  , dziWidth = width
  , dziHeight = height
  , dziTileSize = tileSize
  , dziTileOverlap = tileOverlap
  , dziTileFormat = tileFormat
  }

-- JSON
instance ToJSON DeepZoomImage where
   toJSON = genericToJSON $ aesonPrefix camelCase
instance FromJSON DeepZoomImage where
   parseJSON = genericParseJSON $ aesonPrefix camelCase
