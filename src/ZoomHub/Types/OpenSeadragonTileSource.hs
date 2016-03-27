{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.OpenSeadragonTileSource
  ( OpenSeadragonTileSource
  , fromDeepZoomImage
  ) where

import           Data.Aeson                      (ToJSON, object, toJSON, (.=))
import qualified Data.Text                       as T
import           System.FilePath.Posix           (dropExtension)

import           ZoomHub.API.Types.DeepZoomImage (DeepZoomImage, dziHeight,
                                                  dziTileFormat, dziTileFormat,
                                                  dziTileOverlap, dziTileSize,
                                                  dziUrl, dziWidth)


newtype OpenSeadragonTileSource = OpenSeadragonTileSource
  { unOpenSeadragonTileSource :: DeepZoomImage } deriving (Eq, Show)

fromDeepZoomImage :: DeepZoomImage -> OpenSeadragonTileSource
fromDeepZoomImage = OpenSeadragonTileSource

-- JSON
instance ToJSON OpenSeadragonTileSource where
  toJSON o =
    object ["Image" .=
      object
      [ "xmlns" .= xmlns
      , "Url" .= (dropExtension (show $ dziUrl dzi) ++ "_files/")
      , "Format" .= dziTileFormat dzi
      , "Overlap" .= dziTileOverlap dzi
      , "TileSize" .= dziTileSize dzi
      , "Size" .= object
        [ "Width" .= dziWidth dzi
        , "Height" .= dziHeight dzi
        ]
      ]
    ]
    where
      xmlns :: T.Text
      xmlns = "http://schemas.microsoft.com/deepzoom/2008"

      dzi = unOpenSeadragonTileSource o
