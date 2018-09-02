{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Web.Types.OpenSeadragonViewerConfig
  ( OpenSeadragonViewerConfig
  , mkOpenSeadragonViewerConfig
  ) where

import           Data.Aeson                                (ToJSON, object,
                                                            toJSON, (.=))
import           Data.Maybe                                (fromJust)
import qualified Data.Text                                 as T
import           Network.URI                               (URI, parseRelativeReference,
                                                            relativeTo)
import           System.FilePath                           (addTrailingPathSeparator)

import           ZoomHub.Types.StaticBaseURI               (StaticBaseURI,
                                                            unStaticBaseURI)
import           ZoomHub.Web.Types.OpenSeadragonTileSource (OpenSeadragonTileSource)

data OpenSeadragonViewerConfig = OpenSeadragonViewerConfig
  { osvcContainerId :: String
  , osvcPrefixURI   :: URI
  , oscvTileSource  :: OpenSeadragonTileSource
  } deriving (Eq, Show)

mkOpenSeadragonViewerConfig ::
     StaticBaseURI
  -> String
  -> OpenSeadragonTileSource
  -> OpenSeadragonViewerConfig
mkOpenSeadragonViewerConfig staticBaseURI containerId tileSource =
  OpenSeadragonViewerConfig
    { osvcContainerId = containerId
    , oscvTileSource = tileSource
    , osvcPrefixURI = prefixURI
    }
  where
    prefixURI = prefixPath `relativeTo` unStaticBaseURI staticBaseURI
    prefixPath = fromJust . parseRelativeReference $ "openseadragon-images"

-- JSON
instance ToJSON OpenSeadragonViewerConfig where
  toJSON o =
    object
      [ "id" .= T.pack (osvcContainerId o)
      , "prefixUrl" .=
        (T.pack . addTrailingPathSeparator . show . osvcPrefixURI) o
      , "tileSources" .= toJSON (oscvTileSource o) -- NOTE: plural key
      ]
