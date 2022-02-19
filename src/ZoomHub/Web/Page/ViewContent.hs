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
import Lucid
  ( ToHtml,
    a_,
    class_,
    div_,
    href_,
    script_,
    src_,
    toHtml,
    toHtmlRaw,
  )
import Network.URI (parseRelativeReference, relativeTo)
import ZoomHub.API.Types.Content (Content, contentId, contentUrl)
import ZoomHub.Types.BaseURI (BaseURI, unBaseURI)
import ZoomHub.Types.ContentId (unContentId)
import ZoomHub.Web.Page (Path (..), Title (..))
import qualified ZoomHub.Web.Page as Page

data ViewContent = ViewContent
  { vcContent :: Content,
    vcBaseURI :: BaseURI
  }
  deriving (Eq, Show)

mkViewContent :: BaseURI -> Content -> ViewContent
mkViewContent vcBaseURI vcContent = ViewContent {..}

instance ToHtml ViewContent where
  toHtml vc = Page.layout
    (Title $ T.pack cId <> " — " <> Page.title)
    (Just $ Path $ "/" <> T.pack cId)
    $ do
      script_ [src_ (T.pack $ show scriptURI)] emptyScriptBody
      div_ [class_ "meta"] $
        a_ [href_ (T.pack rawContentURL)] (toHtml rawContentURL)
    where
      content = vcContent vc
      cId = unContentId $ contentId content
      baseURI = vcBaseURI vc

      scriptURI = scriptPath `relativeTo` unBaseURI baseURI
      scriptPath =
        fromJust . parseRelativeReference $
          fold
            [ "/",
              cId,
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

      emptyScriptBody :: Text
      emptyScriptBody = ""

  toHtmlRaw = toHtml
