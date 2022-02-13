{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Web.Types.OpenSeadragonTileSource
  ( OpenSeadragonTileSource,
    fromDeepZoomImage,
  )
where

import Data.Aeson (ToJSON, object, toJSON, (.=))
import qualified Data.Text as T
import System.FilePath (dropExtension)
import ZoomHub.API.Types.DeepZoomImage
  ( DeepZoomImage,
    dziHeight,
    dziTileFormat,
    dziTileOverlap,
    dziTileSize,
    dziUrl,
    dziWidth,
  )

newtype OpenSeadragonTileSource = OpenSeadragonTileSource
  {unOpenSeadragonTileSource :: DeepZoomImage}
  deriving (Eq, Show)

fromDeepZoomImage :: DeepZoomImage -> OpenSeadragonTileSource
fromDeepZoomImage = OpenSeadragonTileSource

-- JSON
instance ToJSON OpenSeadragonTileSource where
  toJSON o =
    object
      [ "Image"
          .= object
            [ "xmlns" .= ("http://schemas.microsoft.com/deepzoom/2008" :: T.Text),
              "Url" .= (dropExtension (show $ dziUrl dzi) ++ "_files/"),
              "Format" .= dziTileFormat dzi,
              "Overlap" .= dziTileOverlap dzi,
              "TileSize" .= dziTileSize dzi,
              "Size"
                .= object
                  [ "Width" .= dziWidth dzi,
                    "Height" .= dziHeight dzi
                  ]
            ]
      ]
    where
      dzi = unOpenSeadragonTileSource o
