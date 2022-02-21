{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Web.Page.EmbedContent
  ( EmbedContent,
    mkEmbedContent,
  )
where

import Data.Aeson (encode)
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Maybe (fromJust, fromMaybe)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Lucid as L
import NeatInterpolation (text)
import Network.URI (parseRelativeReference, relativeTo)
import ZoomHub.API.Types.Content (Content (contentDzi, contentReady), contentId)
import ZoomHub.API.Types.DeepZoomImage (DeepZoomImageURI (DeepZoomImageURI), mkDeepZoomImage)
import ZoomHub.Types.BaseURI (BaseURI)
import ZoomHub.Types.ContentId (unContentId)
import ZoomHub.Types.DeepZoomImage (TileOverlap (TileOverlap1), TileSize (TileSize256))
import qualified ZoomHub.Types.DeepZoomImage as TileFormat
import ZoomHub.Types.StaticBaseURI (StaticBaseURI, unStaticBaseURI)
import ZoomHub.Web.Page (Page (Page), Path (..), Title (..))
import qualified ZoomHub.Web.Page as Page
import ZoomHub.Web.Types.EmbedConstraint (EmbedConstraint (unEmbedConstraint))
import ZoomHub.Web.Types.EmbedObjectFit (EmbedObjectFit (unEmbedObjectFit))
import ZoomHub.Web.Types.OpenSeadragonTileSource (fromDeepZoomImage)
import ZoomHub.Web.Types.OpenSeadragonViewerConfig (mkOpenSeadragonViewerConfig)

data EmbedContent = EmbedContent
  { ecContent :: Content,
    ecBaseURI :: BaseURI,
    ecObjectFit :: Maybe EmbedObjectFit,
    ecConstraint :: Maybe EmbedConstraint,
    ecStaticBaseURI :: StaticBaseURI
  }
  deriving (Eq, Show)

mkEmbedContent ::
  BaseURI ->
  StaticBaseURI ->
  Content ->
  Maybe EmbedObjectFit ->
  Maybe EmbedConstraint ->
  EmbedContent
mkEmbedContent ecBaseURI ecStaticBaseURI ecContent ecObjectFit ecConstraint =
  EmbedContent {..}

instance L.ToHtml EmbedContent where
  toHtml ec =
    Page.layout
      ( Page
          { pageTitle = Title $ T.pack cId <> " — " <> Page.title,
            pageCanonicalPath = Just $ Path $ "/" <> T.pack cId,
            pageBody = do
              L.script_ [L.src_ (T.pack $ show openSeadragonScriptURI)] ("" :: Text)
              L.div_ [L.id_ containerId, L.style_ "width: 100%; height: 100vh;"] ""
              L.script_
                [text|
                OpenSeadragon($openSeadragonConfig)
              |]
          }
      )
    where
      content = ecContent ec
      cId = unContentId $ contentId content
      staticBaseURI = ecStaticBaseURI ec

      openSeadragonScriptPath =
        fromJust . parseRelativeReference $ "scripts/openseadragon/3.0.0-zoomhub/openseadragon.min.js"
      openSeadragonScriptURI =
        openSeadragonScriptPath `relativeTo` unStaticBaseURI staticBaseURI

      openSeadragonConfig = T.pack $ BLC.unpack $ encode viewerConfig
      maybeDZI = contentDzi content
      queuedDZI =
        mkDeepZoomImage queuedDZIURI 8000 6000 TileSize256 TileOverlap1 TileFormat.PNG
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
          (unEmbedObjectFit <$> ecObjectFit ec)
          (unEmbedConstraint <$> ecConstraint ec)
      containerId = "container" :: Text

  toHtmlRaw = L.toHtml
