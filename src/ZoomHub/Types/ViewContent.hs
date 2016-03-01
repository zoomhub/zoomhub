{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module ZoomHub.Types.ViewContent
  ( ViewContent
  , mkViewContent
  )
  where

import           Data.Maybe                       (fromJust)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import           Lucid                            (ToHtml, body_, charset_, content_,
                                                   doctypehtml_, head_, meta_,
                                                   name_, script_, src_, style_,
                                                   title_, toHtml, toHtmlRaw)
import           Network.URI                      (parseRelativeReference,
                                                   relativeTo)

import           ZoomHub.Types.BaseURI            (BaseURI, unBaseURI)
import           ZoomHub.Types.Content            (Content, contentId)
import           ZoomHub.Types.Internal.ContentId (unId)

data ViewContent = ViewContent
  { vcContent :: Content
  , vcBaseURI :: BaseURI
  } deriving (Eq, Show)

mkViewContent :: BaseURI -> Content -> ViewContent
mkViewContent vcBaseURI vcContent = ViewContent{..}

-- HTML
concatPretty :: [T.Text] -> T.Text
concatPretty = T.intercalate "\n"

-- TODO: Improve how we represent inline styles.
styles :: T.Text
styles = concatPretty
  [ "html, body {"
  , "  background-color: black;"
  , "  color: white;"
  , "  font-family: \"Helvetica Neue\", Helvetica, Arial, sans-serif;"
  , "  height: 100%;"
  , "  margin: 0;"
  , "  padding: 0;"
  , "}"
  ]

-- TODO: Improve how we represent analytics code.
-- TODO: Pass through `UA-XXXXXXXX-X` Google Analytics ID.
analyticsScript :: T.Text
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
                     meta_ [charset_ "utf-8"]
                     meta_ [name_ "viewport",
                            content_ "width=device-width, initial-scale=1"]
                     style_ styles
                     script_ analyticsScript
                     )
           body_ $ script_ [src_ (T.pack $ show scriptURI)] emptyBody
    where
      title = "zoomhub · Share and view high-resolution images effortlessly"
      titleSeparator = " — "
      content = vcContent vc
      cId = unId $ contentId content
      baseURI = vcBaseURI vc
      scriptURI = scriptPath `relativeTo` unBaseURI baseURI
      scriptPath = fromJust . parseRelativeReference $
        "/" ++ cId ++ ".js?id=container&width=auto&height=100" ++ escapedPercent
      escapedPercent = "%25"

      emptyBody :: T.Text
      emptyBody = ""

  toHtmlRaw = toHtml
