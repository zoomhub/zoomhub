{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Web.Page.EmbedContent
  ( EmbedContent (..),
  )
where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Maybe (fromJust, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Lucid as H
import NeatInterpolation (text)
import Network.URI (parseRelativeReference, relativeTo)
import ZoomHub.API.Types.Content (Content (contentDzi, contentReady), contentId)
import ZoomHub.API.Types.DeepZoomImage (DeepZoomImageURI (DeepZoomImageURI), mkDeepZoomImage)
import ZoomHub.Types.BaseURI (BaseURI)
import ZoomHub.Types.ContentId (unContentId)
import ZoomHub.Types.DeepZoomImage (TileOverlap (TileOverlap1), TileSize (TileSize254))
import qualified ZoomHub.Types.DeepZoomImage as TileFormat
import ZoomHub.Types.StaticBaseURI (StaticBaseURI, unStaticBaseURI)
import ZoomHub.Web.Page (Page (Page), Path (..), Title (..))
import qualified ZoomHub.Web.Page as Page
import ZoomHub.Web.Types.EmbedBackground (EmbedBackground)
import qualified ZoomHub.Web.Types.EmbedBackground as EmbedBackground
import ZoomHub.Web.Types.EmbedConstraint (EmbedConstraint (unEmbedConstraint))
import ZoomHub.Web.Types.EmbedObjectFit (EmbedObjectFit (unEmbedObjectFit))
import ZoomHub.Web.Types.OpenSeadragonTileSource (fromDeepZoomImage)
import ZoomHub.Web.Types.OpenSeadragonViewerConfig (mkOpenSeadragonViewerConfig)

data EmbedContent = EmbedContent
  { ecBackgroundColor :: Maybe EmbedBackground,
    ecContent :: Content,
    ecBaseURI :: BaseURI,
    ecConstraint :: Maybe EmbedConstraint,
    ecObjectFit :: Maybe EmbedObjectFit,
    ecStaticBaseURI :: StaticBaseURI
  }
  deriving (Eq, Show)

instance H.ToHtml EmbedContent where
  toHtml EmbedContent {..} =
    Page.layout
      ( Page
          { pageTitle = Title $ T.pack cId <> " — " <> Page.title,
            pageCanonicalPath = Just $ Path $ "/" <> T.pack cId,
            pageHeadStyles = Nothing,
            pageBodyClassName = Nothing,
            pageBody = do
              H.script_
                [H.src_ (T.pack $ show openSeadragonScriptURI)]
                ("" :: Text)
              H.div_
                [ H.id_ containerId,
                  H.style_ $
                    T.intercalate
                      ";"
                      [ "width: 100%",
                        "height: 100vh",
                        "background: " <> EmbedBackground.toCSSValue backgroundColor
                      ]
                ]
                ""
              H.script_
                [text|
                OpenSeadragon($openSeadragonConfig)
              |]
          }
      )
    where
      content = ecContent
      cId = unContentId $ contentId content
      staticBaseURI = ecStaticBaseURI

      backgroundColor = fromMaybe EmbedBackground.Black ecBackgroundColor

      openSeadragonScriptPath =
        fromJust . parseRelativeReference $ "scripts/openseadragon/3.0.0-zoomhub/openseadragon.min.js"
      openSeadragonScriptURI =
        openSeadragonScriptPath `relativeTo` unStaticBaseURI staticBaseURI

      openSeadragonConfig = T.pack $ BLC.unpack $ encode viewerConfig
      maybeDZI = contentDzi content
      queuedDZI =
        mkDeepZoomImage queuedDZIURI 8000 6000 TileSize254 TileOverlap1 TileFormat.PNG
      queuedDZIURI =
        DeepZoomImageURI $ queuedDZIPath `relativeTo` unStaticBaseURI staticBaseURI
      queuedDZIPath = fromJust (parseRelativeReference "queued.dzi")
      isReady = contentReady content
      tileSource
        | not isReady = fromDeepZoomImage queuedDZI
        | otherwise = fromDeepZoomImage $ fromMaybe queuedDZI maybeDZI
      viewerConfig =
        mkOpenSeadragonViewerConfig
          staticBaseURI
          (T.unpack containerId)
          tileSource
          (unEmbedObjectFit <$> ecObjectFit)
          (unEmbedConstraint <$> ecConstraint)
      containerId = "container" :: Text

  toHtmlRaw = H.toHtml
