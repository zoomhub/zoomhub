{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.API.Types.DeepZoomImage
  ( DeepZoomImage (DeepZoomImage),
    DeepZoomImageURI (DeepZoomImageURI),
    dziHeight,
    dziTileFormat,
    dziTileOverlap,
    dziTileSize,
    dziUrl,
    largestSingleTileUrl,
    dziWidth,
    fromInternal,
    toInternal,
    mkDeepZoomImage,
  )
where

import Data.Aeson (FromJSON, ToJSON, Value (String), genericParseJSON, genericToJSON, parseJSON, toJSON, withText)
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)
import qualified Generics.SOP as SOP
import Network.URI (URI, parseAbsoluteURI, parseRelativeReference, relativeTo)
import System.FilePath ((<.>))
import ZoomHub.Types.ContentBaseURI
  ( ContentBaseURI,
    unContentBaseURI,
  )
import ZoomHub.Types.ContentId (ContentId, unContentId)
import qualified ZoomHub.Types.DeepZoomImage as Internal
import ZoomHub.Utils (tshow)

data DeepZoomImage = DeepZoomImage
  { dziUrl :: DeepZoomImageURI,
    dziWidth :: Integer,
    dziHeight :: Integer,
    dziTileSize :: Internal.TileSize,
    dziTileOverlap :: Internal.TileOverlap,
    dziTileFormat :: Internal.TileFormat
  }
  deriving (Eq, Show, Generic)

fromInternal ::
  ContentBaseURI ->
  ContentId ->
  Internal.DeepZoomImage ->
  Maybe DeepZoomImage
fromInternal baseURI cId dzi = do
  namePath <- parseRelativeReference name
  let url = namePath `relativeTo` unContentBaseURI baseURI
  pure
    DeepZoomImage
      { dziUrl = DeepZoomImageURI url,
        dziWidth = Internal.dziWidth dzi,
        dziHeight = Internal.dziHeight dzi,
        dziTileSize = Internal.dziTileSize dzi,
        dziTileOverlap = Internal.dziTileOverlap dzi,
        dziTileFormat = Internal.dziTileFormat dzi
      }
  where
    name = unContentId cId <.> "dzi"

toInternal :: DeepZoomImage -> Internal.DeepZoomImage
toInternal dzi =
  Internal.mkDeepZoomImage
    (dziWidth dzi)
    (dziHeight dzi)
    (dziTileSize dzi)
    (dziTileOverlap dzi)
    (dziTileFormat dzi)

mkDeepZoomImage ::
  DeepZoomImageURI ->
  Integer ->
  Integer ->
  Internal.TileSize ->
  Internal.TileOverlap ->
  Internal.TileFormat ->
  DeepZoomImage
mkDeepZoomImage uri width height tileSize tileOverlap tileFormat =
  DeepZoomImage
    { dziUrl = uri,
      dziWidth = width,
      dziHeight = height,
      dziTileSize = tileSize,
      dziTileOverlap = tileOverlap,
      dziTileFormat = tileFormat
    }

-- TODO: Make dynamic based on size of image
largestSingleTileUrl :: DeepZoomImage -> URI
largestSingleTileUrl dzi =
  fromMaybe
    (error "Invalid DZI tile URL")
    $ parseAbsoluteURI
      . T.unpack
      $ T.dropEnd
        (T.length dziExtension)
        (tshow $ dziUrl dzi)
        <> "_files/"
        <> tshow largestSingleTileLevel
        <> "/0_0."
        <> tshow (dziTileFormat dzi)
  where
    dziExtension = ".dzi"
    largestSingleTileLevel = 8 :: Int

-- JSON
instance ToJSON DeepZoomImage where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON DeepZoomImage where
  parseJSON = genericParseJSON $ aesonPrefix camelCase

-- Types
newtype DeepZoomImageURI = DeepZoomImageURI {unDeepZoomImageURI :: URI}
  deriving (Eq)

instance Show DeepZoomImageURI where
  show = show . unDeepZoomImageURI

instance ToJSON DeepZoomImageURI where
  toJSON = String . T.pack . show . unDeepZoomImageURI

instance FromJSON DeepZoomImageURI where
  parseJSON = withText "DeepZoomImageURI" $ \text ->
    case parseAbsoluteURI $ T.unpack text of
      Just uri -> pure $ DeepZoomImageURI uri
      Nothing -> fail "invalid URI"

-- PostgreSQL / Squeal
instance SOP.Generic DeepZoomImage

instance SOP.HasDatatypeInfo DeepZoomImage
