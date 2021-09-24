{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Web.Page
  ( template,
    title,
    styles,
    analyticsScript,
    -- TODO: Hide
    concatPretty,
  )
where

import Data.Text (Text)
import qualified Data.Text as T
import Lucid
  ( HtmlT,
    body_,
    charset_,
    content_,
    doctypehtml_,
    head_,
    href_,
    link_,
    meta_,
    name_,
    rel_,
    script_,
    sizes_,
    style_,
    title_,
    toHtml,
  )

title :: Text
title = "ZoomHub · Share and view high-resolution images effortlessly"

template ::
  Monad m =>
  Text -> -- title
  HtmlT m a ->
  HtmlT m a
template pageTitle body = do
  doctypehtml_ $
    do
      head_
        ( do
            title_ (toHtml pageTitle)
            meta_ [charset_ "utf-8"]
            meta_
              [ name_ "viewport",
                content_ "width=device-width, initial-scale=1"
              ]
            link_
              [ rel_ "shortcut icon",
                href_ "favicon.ico"
              ]
            link_
              [ rel_ "apple-touch-icon",
                href_ "/apple-touch-icon.png"
              ]
            link_
              [ rel_ "apple-touch-icon-precomposed",
                href_ "/apple-touch-icon-precomposed.png"
              ]
            link_
              [ rel_ "apple-touch-icon",
                sizes_ "57x57",
                href_ "/apple-touch-icon-57x57.png"
              ]
            link_
              [ rel_ "apple-touch-icon",
                sizes_ "76x76",
                href_ "/apple-touch-icon-76x76.png"
              ]
            link_
              [ rel_ "apple-touch-icon",
                sizes_ "120x120",
                href_ "/apple-touch-icon-120x120.png"
              ]
            link_
              [ rel_ "apple-touch-icon",
                sizes_ "152x152",
                href_ "/apple-touch-icon-152x152.png"
              ]
            link_
              [ rel_ "apple-touch-icon",
                sizes_ "167x167",
                href_ "/apple-touch-icon-167x167.png"
              ]
            link_
              [ rel_ "apple-touch-icon",
                sizes_ "180x180",
                href_ "/apple-touch-icon-180x180.png"
              ]
            style_ styles
            script_ analyticsScript
        )
      body_ $ body

-- HTML
concatPretty :: [Text] -> Text
concatPretty = T.intercalate "\n"

-- TODO: Improve how we represent inline styles.
styles :: Text
styles =
  concatPretty
    [ "html, body {",
      "  background-color: black;",
      "  color: white;",
      "  font-family: \"Helvetica Neue\", Helvetica, Arial, sans-serif;",
      "  font-size: 16px;",
      "  height: 100%;",
      "  margin: 0;",
      "  padding: 0;",
      "}",
      ".meta {",
      "  align-items: center;",
      "  bottom: 0;",
      "  color: white;",
      "  display: flex;",
      "  font-size: 12px;",
      "  justify-content: center;",
      "  left: 0;",
      "  padding: 1em 3em;",
      "  position: fixed;",
      "  right: 0;",
      "}",
      ".meta a,",
      ".meta a:visited {",
      "  font-family: monospace;",
      "  color: #666;",
      "}",
      ".meta a:hover {",
      "  color: #ddd;",
      "}",
      "a,",
      ".a:visited {",
      "  font-family: monospace;",
      "  color: #f60;",
      "}",
      ".meta a:hover {",
      "  color: #fff;",
      "  text-decoration: underline;",
      "}"
    ]

-- TODO: Improve how we represent analytics code.
-- TODO: Pass through `UA-XXXXXXXX-X` Google Analytics ID.
analyticsScript :: Text
analyticsScript =
  concatPretty
    [ "(function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){",
      "(i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),",
      "m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)",
      "})(window,document,'script','//www.google-analytics.com/analytics.js','ga');",
      "",
      "ga('create', 'UA-55808703-1', 'auto');",
      "ga('_setDomainName', 'zoomhub.net');",
      "ga('_setAllowLinker', true);",
      "ga('send', 'pageview');"
    ]
