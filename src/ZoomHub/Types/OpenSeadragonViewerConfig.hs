{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.OpenSeadragonViewerConfig
  ( OpenSeadragonViewerConfig
  , mkOpenSeadragonViewerConfig
  ) where

import           Data.Aeson                            (ToJSON, object, toJSON,
                                                        (.=))
import           Data.Maybe                            (fromJust)
import qualified Data.Text                             as T
import           Network.URI                           (URI,
                                                        parseRelativeReference,
                                                        relativeTo)

import           ZoomHub.Types.ContentBaseURI          (ContentBaseURI,
                                                        unContentBaseURI)
import           ZoomHub.Types.OpenSeadragonTileSource (OpenSeadragonTileSource)


data OpenSeadragonViewerConfig = OpenSeadragonViewerConfig
  { osvcContainerId :: String
  , osvcPrefixURI   :: URI
  , oscvTileSource  :: OpenSeadragonTileSource
  } deriving (Eq, Show)

mkOpenSeadragonViewerConfig :: ContentBaseURI ->
                               String ->
                               OpenSeadragonTileSource ->
                               OpenSeadragonViewerConfig
mkOpenSeadragonViewerConfig cbu i ts = OpenSeadragonViewerConfig
  { osvcContainerId = i
  , oscvTileSource = ts
  , osvcPrefixURI = prefixURI
  }
  where
    prefixURI = prefixPath `relativeTo` unContentBaseURI cbu
    prefixPath = fromJust . parseRelativeReference $ "/openseadragon-images/"

-- JSON
instance ToJSON OpenSeadragonViewerConfig where
  toJSON o =
    object
    [ "id" .= T.pack (osvcContainerId o)
    , "prefixUrl" .= (T.pack . show . osvcPrefixURI) o
    , "tileSources" .= toJSON (oscvTileSource o) -- NOTE: plural key
    ]
