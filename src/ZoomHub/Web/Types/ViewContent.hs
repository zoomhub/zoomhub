{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Web.Types.ViewContent
  ( ViewContent
  , mkViewContent
  )
  where

import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Lucid
  ( ToHtml
  , a_
  , body_
  , charset_
  , class_
  , content_
  , div_
  , doctypehtml_
  , head_
  , href_
  , link_
  , meta_
  , name_
  , rel_
  , script_
  , sizes_
  , src_
  , style_
  , title_
  , toHtml
  , toHtmlRaw
  )
import Network.URI (parseRelativeReference, relativeTo)

import           ZoomHub.API.Types.Content (Content, contentId, contentUrl)
import           ZoomHub.Types.BaseURI     (BaseURI, unBaseURI)
import           ZoomHub.Types.ContentId   (unContentId)

data ViewContent = ViewContent
  { vcContent :: Content
  , vcBaseURI :: BaseURI
  } deriving (Eq, Show)

mkViewContent :: BaseURI -> Content -> ViewContent
mkViewContent vcBaseURI vcContent = ViewContent{..}

-- HTML
concatPretty :: [Text] -> Text
concatPretty = T.intercalate "\n"

-- TODO: Improve how we represent inline styles.
styles :: Text
styles = concatPretty
  [ "html, body {"
  , "  background-color: black;"
  , "  color: white;"
  , "  font-family: \"Helvetica Neue\", Helvetica, Arial, sans-serif;"
  , "  font-size: 12px;"
  , "  height: 100%;"
  , "  margin: 0;"
  , "  padding: 0;"
  , "}"
  , ".meta {"
  , "  align-items: center;"
  , "  bottom: 0;"
  , "  color: white;"
  , "  display: flex;"
  , "  justify-content: center;"
  , "  left: 0;"
  , "  padding: 1em 3em;"
  , "  position: fixed;"
  , "  right: 0;"
  , "}"
  , ".meta a,"
  , ".meta a:visited {"
  , "  font-family: monospace;"
  , "  color: #666;"
  , "}"
  , ".meta a:hover {"
  , "  color: #ddd;"
  , "}"
  ]

-- TODO: Improve how we represent analytics code.
-- TODO: Pass through `UA-XXXXXXXX-X` Google Analytics ID.
analyticsScript :: Text
analyticsScript = concatPretty [
    "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){",
    "(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),",
    "m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)",
    "})(window,document,'script','//www.google-analytics.com/analytics.js','ga');",
    "",
    "ga('create', 'UA-55808703-1', 'auto');",
    "ga('_setDomainName', 'zoomhub.net');",
    "ga('_setAllowLinker', true);",
    "ga('send', 'pageview');"
  ]

instance ToHtml ViewContent where
  toHtml vc =
      doctypehtml_ $
        do head_ (do title_ (toHtml $ cId <> titleSeparator <> title)
                     meta_ [ charset_ "utf-8" ]
                     meta_ [ name_ "viewport"
                           , content_ "width=device-width, initial-scale=1"
                           ]
                     link_ [ rel_ "shortcut icon"
                           , href_ "favicon.ico"]

                     link_ [ rel_ "apple-touch-icon"
                           , href_ "/apple-touch-icon.png"]
                     link_ [ rel_ "apple-touch-icon-precomposed"
                           , href_ "/apple-touch-icon-precomposed.png"]
                     link_ [ rel_ "apple-touch-icon"
                           , sizes_ "57x57"
                           , href_ "/apple-touch-icon-57x57.png"]
                     link_ [ rel_ "apple-touch-icon"
                           , sizes_ "76x76"
                           , href_ "/apple-touch-icon-76x76.png"]
                     link_ [ rel_ "apple-touch-icon"
                           , sizes_ "120x120"
                           , href_ "/apple-touch-icon-120x120.png"]
                     link_ [ rel_ "apple-touch-icon"
                           , sizes_ "152x152"
                           , href_ "/apple-touch-icon-152x152.png"]
                     link_ [ rel_ "apple-touch-icon"
                           , sizes_ "167x167"
                           , href_ "/apple-touch-icon-167x167.png"]
                     link_ [ rel_ "apple-touch-icon"
                           , sizes_ "180x180"
                           , href_ "/apple-touch-icon-180x180.png"]

                     style_ styles
                     script_ analyticsScript
                     )
           body_ $ do
            script_ [src_ (T.pack $ show scriptURI)] emptyScriptBody
            div_ [ class_ "meta" ] $
              a_ [ href_ (T.pack rawContentURL) ] (toHtml rawContentURL)

    where
      title = "ZoomHub · Share and view high-resolution images effortlessly"
      titleSeparator = " — "
      content = vcContent vc
      cId = unContentId $ contentId content
      baseURI = vcBaseURI vc
      scriptURI = scriptPath `relativeTo` unBaseURI baseURI
      scriptPath = fromJust . parseRelativeReference $
        "/" ++ cId ++ ".js?id=container&width=auto&height=100" ++ escapedPercent
      escapedPercent = "%25"
      rawContentURL = show . contentUrl $ content

      emptyScriptBody :: Text
      emptyScriptBody = ""

  toHtmlRaw = toHtml
