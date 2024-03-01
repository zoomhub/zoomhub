{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Web.Types.Embed
  ( Embed (..),
  )
where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.List (intercalate)
import Data.Maybe (fromJust, fromMaybe)
import qualified Data.Text as T
import GHC.Generics (Generic)
import NeatInterpolation (text)
import Network.URI (parseRelativeReference, relativeTo)
import ZoomHub.API.ContentTypes.JavaScript (ToJS, toJS)
import ZoomHub.API.Types.Content (Content, contentDzi, contentReady)
import qualified ZoomHub.API.Types.Content as API
import ZoomHub.API.Types.DeepZoomImage (DeepZoomImageURI (..), mkDeepZoomImage)
import qualified ZoomHub.API.Types.DeepZoomImage as API
import ZoomHub.Types.BaseURI (BaseURI)
import ZoomHub.Types.DeepZoomImage
  ( TileFormat (PNG),
    TileOverlap (TileOverlap1),
    TileSize (TileSize256),
  )
import ZoomHub.Types.StaticBaseURI (StaticBaseURI, unStaticBaseURI)
import qualified ZoomHub.Web.Types.EmbedAspectRatio as EmbedAspectRatio
import ZoomHub.Web.Types.EmbedBackground (EmbedBackground)
import qualified ZoomHub.Web.Types.EmbedBackground as EmbedBackground
import qualified ZoomHub.Web.Types.EmbedBackground as EmbedBackgroundColor
import ZoomHub.Web.Types.EmbedBorder
import qualified ZoomHub.Web.Types.EmbedBorder as EmbedBorder
import ZoomHub.Web.Types.EmbedConstraint (EmbedConstraint (unEmbedConstraint))
import ZoomHub.Web.Types.EmbedDimension (EmbedDimension (..))
import qualified ZoomHub.Web.Types.EmbedDimension as EmbedDimension
import ZoomHub.Web.Types.EmbedObjectFit (EmbedObjectFit (unEmbedObjectFit))
import ZoomHub.Web.Types.OpenSeadragonTileSource (fromDeepZoomImage)
import ZoomHub.Web.Types.OpenSeadragonViewerConfig (mkOpenSeadragonViewerConfig)

data Embed = Embed
  { embedBaseURI :: BaseURI,
    embedStaticBaseURI :: StaticBaseURI,
    embedBody :: String,
    embedContainerId :: String,
    embedContent :: Content,
    embedHeight :: Maybe EmbedDimension,
    embedWidth :: Maybe EmbedDimension,
    embedBorder :: Maybe EmbedBorder,
    embedObjectFit :: Maybe EmbedObjectFit,
    embedConstraint :: Maybe EmbedConstraint,
    embedBackgroundColor :: Maybe EmbedBackground
  }
  deriving (Eq, Generic, Show)

-- CSS
legacyCSSClassName :: String
legacyCSSClassName = "__seadragon"

cssClassNames :: [String]
cssClassNames = ["__zoomhub", legacyCSSClassName]

defaultHeight :: EmbedDimension
defaultHeight = Pixels 200

defaultWidth :: EmbedDimension
defaultWidth = Auto

style :: Embed -> String
style Embed {embedContent, embedWidth, embedHeight, embedBorder, embedBackgroundColor} =
  T.unpack $
    T.intercalate
      ";"
      [ "aspect-ratio: " <> EmbedAspectRatio.toCSSValue aspectRatio,
        "background: " <> EmbedBackground.toCSSValue background,
        "border: " <> EmbedBorder.toCSSValue border,
        "color: white",
        "height: " <> EmbedDimension.toCSSValue height,
        "margin: 0",
        "padding: 0",
        "width: " <> EmbedDimension.toCSSValue width
      ]
  where
    background = fromMaybe EmbedBackground.Black embedBackgroundColor
    border = fromMaybe EmbedBorder.Default embedBorder
    height = fromMaybe defaultHeight embedHeight
    width = fromMaybe defaultWidth embedWidth
    aspectRatio =
      case API.contentDzi embedContent of
        Just dzi ->
          EmbedAspectRatio.Ratio
            (API.dziWidth dzi)
            (API.dziHeight dzi)
        Nothing ->
          EmbedAspectRatio.Auto

-- JavaScript
concatScripts :: [String] -> String
concatScripts = intercalate ";\n"

tag :: String -> [(String, String)] -> String
tag name attrs =
  "<" <> name <> " " <> unwords (map attr attrs) <> "></" <> name <> ">"

attr :: (String, String) -> String
attr (name, value) = concat [name, "=", "\"", value, "\""]

-- TODO: Refactor using Lucid
instance ToJS Embed where
  toJS embed@Embed {..} = concatScripts [script, T.unpack wrapper]
    where
      html =
        T.pack $
          "'"
            <> tag
              "div"
              [ ("class", unwords cssClassNames),
                ("id", embedContainerId),
                ("style", style embed)
              ]
            <> "'"
      wrapper =
        [text|
          ;(() => {
            document.write($html)
            const viewer = OpenSeadragon($openSeadragonConfig)
            viewer.addHandler("full-page", ({fullPage}) => {
              viewer.canvas.style.backgroundColor =
                fullPage ? "black" : "$backgroundColor"
            })
          })()
        |]
      openSeadragonConfig = T.pack $ BLC.unpack $ encode viewerConfig
      backgroundColor =
        EmbedBackground.toCSSValue $
          fromMaybe EmbedBackgroundColor.Black embedBackgroundColor
      script = embedBody
      content = embedContent
      maybeDZI = contentDzi content
      queuedDZI =
        mkDeepZoomImage queuedDZIURI 8000 6000 TileSize256 TileOverlap1 PNG
      queuedDZIURI =
        DeepZoomImageURI $
          queuedDZIPath `relativeTo` unStaticBaseURI embedStaticBaseURI
      queuedDZIPath = fromJust (parseRelativeReference "queued.dzi")
      isReady = contentReady content
      tileSource
        | not isReady = fromDeepZoomImage queuedDZI
        | otherwise = fromDeepZoomImage $ fromMaybe queuedDZI maybeDZI
      viewerConfig =
        mkOpenSeadragonViewerConfig
          embedStaticBaseURI
          embedContainerId
          tileSource
          (unEmbedObjectFit <$> embedObjectFit)
          (unEmbedConstraint <$> embedConstraint)
