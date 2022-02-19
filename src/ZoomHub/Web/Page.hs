{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Web.Page
  ( Page (..),
    layout,
    Title (..),
    Path (..),
    title,
    styles,
    analyticsScript,
  )
where

import Control.Monad (forM_)
import Data.Text (Text)
import Lucid (ToHtml (toHtml))
import qualified Lucid as H
import NeatInterpolation (text)

title :: Text
title = "ZoomHub · Share and view full-resolution images easily"

newtype Title = Title Text

instance H.ToHtml Title where
  toHtml (Title t) = H.toHtml t
  toHtmlRaw (Title t) = H.toHtmlRaw t

newtype Path = Path Text

instance H.ToHtml Path where
  toHtml (Path p) = H.toHtml p
  toHtmlRaw (Path p) = H.toHtmlRaw p

data Page m a = Page
  { pageTitle :: Title,
    pageCanonicalPath :: Maybe Path,
    pageBody :: H.HtmlT m a
  }

layout :: Monad m => Page m a -> H.HtmlT m a
layout Page {..} = do
  H.doctypehtml_ $
    do
      H.head_ do
        H.meta_ [H.charset_ "utf-8"]
        H.title_ (H.toHtml pageTitle)
        forM_ pageCanonicalPath $ \(Path path) ->
          H.link_ [H.rel_ "canonical", H.href_ $ "https://zoomhub.net" <> path]
        H.meta_ [H.name_ "viewport", H.content_ "width=device-width, initial-scale=1"]
        H.link_ [H.rel_ "shortcut icon", H.href_ "favicon.ico"]
        appleTouchIcons
        H.style_ styles
        H.script_ analyticsScript
      H.body_ pageBody

-- TODO: Improve how we represent inline styles.
styles :: Text
styles =
  [text|
    html, body {
      background-color: black;
      color: white;
      font-family: "Helvetica Neue", Helvetica, Arial, sans-serif;
      font-size: 16px;
      height: 100%;
      margin: 0;
      padding: 0;
    }
    .meta {
      align-items: center;
      bottom: 0;
      color: white;
      display: flex;
      font-size: 12px;
      justify-content: center;
      left: 0;
      padding: 1em 3em;
      position: fixed;
      right: 0;
    }
    .meta a,
    .meta a:visited {
      font-family: monospace;
      color: #666;
    }
    .meta a:hover {
      color: #ddd;
    }
    a,
    .a:visited {
      color: #f60;
    }
    a:hover {
      color: #fff;
      text-decoration: underline;
    }
  |]

-- TODO: Improve how we represent analytics code.
-- TODO: Pass through `UA-XXXXXXXX-X` Google Analytics ID.
analyticsScript :: Text
analyticsScript =
  [text|
    (function(i,s,o,g,r,a,m){i['GoogleAnalyticsObject']=r;i[r]=i[r]||function(){
    (i[r].q=i[r].q||[]).push(arguments)},i[r].l=1*new Date();a=s.createElement(o),
    m=s.getElementsByTagName(o)[0];a.async=1;a.src=g;m.parentNode.insertBefore(a,m)
    })(window,document,'script','//www.google-analytics.com/analytics.js','ga');

    ga('create', 'UA-55808703-1', 'auto');
    ga('_setDomainName', 'zoomhub.net');
    ga('_setAllowLinker', true);
    ga('send', 'pageview');
  |]

appleTouchIcons :: Monad m => H.HtmlT m ()
appleTouchIcons = do
  H.link_
    [ H.rel_ "apple-touch-icon",
      H.href_ "/apple-touch-icon.png"
    ]
  H.link_
    [ H.rel_ "apple-touch-icon-precomposed",
      H.href_ "/apple-touch-icon-precomposed.png"
    ]
  H.link_
    [ H.rel_ "apple-touch-icon",
      H.sizes_ "57x57",
      H.href_ "/apple-touch-icon-57x57.png"
    ]
  H.link_
    [ H.rel_ "apple-touch-icon",
      H.sizes_ "76x76",
      H.href_ "/apple-touch-icon-76x76.png"
    ]
  H.link_
    [ H.rel_ "apple-touch-icon",
      H.sizes_ "120x120",
      H.href_ "/apple-touch-icon-120x120.png"
    ]
  H.link_
    [ H.rel_ "apple-touch-icon",
      H.sizes_ "152x152",
      H.href_ "/apple-touch-icon-152x152.png"
    ]
  H.link_
    [ H.rel_ "apple-touch-icon",
      H.sizes_ "167x167",
      H.href_ "/apple-touch-icon-167x167.png"
    ]
  H.link_
    [ H.rel_ "apple-touch-icon",
      H.sizes_ "180x180",
      H.href_ "/apple-touch-icon-180x180.png"
    ]
