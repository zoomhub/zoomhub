{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.OpenSeadragonViewerConfig
  ( OpenSeadragonViewerConfig
  , mkOpenSeadragonViewerConfig
  ) where

import           Data.Aeson                            (ToJSON, object, toJSON,
                                                        (.=))
import qualified Data.Text                             as T

import           ZoomHub.Types.OpenSeadragonTileSource (OpenSeadragonTileSource)


data OpenSeadragonViewerConfig = OpenSeadragonViewerConfig
  { osvcContainerId :: String
  , osvcPrefixURL   :: String
  , oscvTileSource  :: OpenSeadragonTileSource
  } deriving (Eq, Show)

mkOpenSeadragonViewerConfig :: String ->
                               OpenSeadragonTileSource ->
                               OpenSeadragonViewerConfig
mkOpenSeadragonViewerConfig i ts = OpenSeadragonViewerConfig
  { osvcContainerId = i
  , oscvTileSource = ts
  -- TODO: Make configurable:
  , osvcPrefixURL = "http://content.zoomhub.net/openseadragon-images/"
  }

-- JSON
instance ToJSON OpenSeadragonViewerConfig where
  toJSON o =
    object
    [ "id" .= T.pack (osvcContainerId o)
    , "prefixUrl" .= T.pack (osvcPrefixURL o)
    , "tileSources" .= toJSON (oscvTileSource o) -- NOTE: plural key
    ]
