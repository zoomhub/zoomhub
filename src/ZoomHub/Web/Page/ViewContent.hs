{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Web.Page.ViewContent
  ( ViewContent,
    mkViewContent,
  )
where

import Data.Foldable (fold)
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Lucid as H
import Network.URI (parseRelativeReference, relativeTo)
import ZoomHub.API.Types.Content (Content, contentId, contentUrl)
import ZoomHub.Types.BaseURI (BaseURI, unBaseURI)
import ZoomHub.Types.ContentId (unContentId)
import ZoomHub.Web.Page (Page (Page), Path (..), Title (..))
import qualified ZoomHub.Web.Page as Page

data ViewContent = ViewContent
  { vcContent :: Content,
    vcBaseURI :: BaseURI
  }
  deriving (Eq, Show)

mkViewContent :: BaseURI -> Content -> ViewContent
mkViewContent vcBaseURI vcContent = ViewContent {..}

instance H.ToHtml ViewContent where
  toHtml vc =
    Page.layout $
      Page
        { pageTitle = Title $ T.pack cId <> " — " <> Page.title,
          pageCanonicalPath = Just $ Path $ "/" <> T.pack cId,
          pageBody = do
            H.script_ [H.src_ (T.pack $ show scriptURI)] ("" :: Text)
            H.div_
              [H.class_ "fixed left-0 bottom-0 right-0 px-2 py-3 flex justify-center items-center text-sm text-white"]
              $ H.a_
                [H.class_ "font-mono text-gray-600 hover:text-gray-200", H.href_ (T.pack rawContentURL)]
                (H.toHtml rawContentURL)
        }
    where
      content = vcContent vc
      cId = unContentId $ contentId content
      baseURI = vcBaseURI vc

      scriptURI = scriptPath `relativeTo` unBaseURI baseURI
      scriptPath =
        fromJust . parseRelativeReference $
          fold
            [ cId,
              ".js",
              "?",
              "id=container",
              "&",
              "width=100" <> escapedPercent,
              "&",
              "height=100" <> escapedPercent
            ]
      escapedPercent = "%25"
      rawContentURL = show . contentUrl $ content

  toHtmlRaw = H.toHtml
