{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Web.Types.OpenSeadragonViewerConfig
  ( OpenSeadragonViewerConfig,
    mkOpenSeadragonViewerConfig,
    ObjectFit (..),
    Constraint (..),
  )
where

import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import Network.URI (URI, parseRelativeReference, relativeTo)
import System.FilePath (addTrailingPathSeparator)
import ZoomHub.Types.StaticBaseURI (StaticBaseURI, unStaticBaseURI)
import ZoomHub.Web.Types.OpenSeadragonTileSource (OpenSeadragonTileSource)

data ObjectFit = Contain | Cover
  deriving (Eq, Show)

data Constraint = Zoom | Full
  deriving (Eq, Show)

data OpenSeadragonViewerConfig = OpenSeadragonViewerConfig
  { osvcContainerId :: String,
    osvcPrefixURI :: URI,
    oscvTileSource :: OpenSeadragonTileSource,
    oscvObjectFit :: ObjectFit,
    oscvConstraint :: Maybe Constraint
  }
  deriving (Eq, Show)

mkOpenSeadragonViewerConfig ::
  StaticBaseURI ->
  String ->
  OpenSeadragonTileSource ->
  Maybe ObjectFit ->
  Maybe Constraint ->
  OpenSeadragonViewerConfig
mkOpenSeadragonViewerConfig staticBaseURI containerId tileSource objectFit constraint =
  OpenSeadragonViewerConfig
    { osvcContainerId = containerId,
      oscvTileSource = tileSource,
      osvcPrefixURI = prefixURI,
      oscvObjectFit = fromMaybe Contain objectFit,
      oscvConstraint = constraint
    }
  where
    prefixURI = prefixPath `relativeTo` unStaticBaseURI staticBaseURI
    prefixPath = fromJust . parseRelativeReference $ "scripts/openseadragon/3.0.0-zoomhub/images"

-- JSON
instance ToJSON OpenSeadragonViewerConfig where
  toJSON o =
    object $
      [ "id" .= T.pack (osvcContainerId o),
        "prefixUrl" .= (T.pack . addTrailingPathSeparator . show . osvcPrefixURI) o,
        "tileSources" .= toJSON (oscvTileSource o), -- NOTE: plural key
        "homeFillsViewer" .= case oscvObjectFit o of
          Contain -> False
          Cover -> True
      ]
        <> case oscvConstraint o of
          Nothing ->
            []
          Just Zoom ->
            ["minZoomImageRatio" .= (1.0 :: Double)]
          Just Full ->
            [ "minZoomImageRatio" .= (1.0 :: Double),
              "visibilityRatio" .= (1.0 :: Double),
              "constrainDuringPan" .= True
            ]
