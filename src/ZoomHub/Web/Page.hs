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
    titleShort,
    analyticsScript,
  )
where

import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Lucid as H
import NeatInterpolation (text)
import qualified ZoomHub.Config as Config

title :: Text
title = "ZoomHub · Share and view full-resolution images easily"

titleShort :: Text
titleShort = "ZoomHub"

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
    pageHeadStyles :: Maybe (H.HtmlT m a),
    pageBody :: H.HtmlT m a,
    pageBodyClassName :: Maybe Text
  }

layout :: (Monad m) => Page m a -> H.HtmlT m a
layout Page {..} = do
  H.doctype_
  H.html_ [H.lang_ "en", H.class_ "h-full"] do
    H.head_ do
      H.meta_ [H.charset_ "utf-8"]
      H.title_ (H.toHtml pageTitle)
      forM_ pageCanonicalPath $ \(Path path) ->
        H.link_ [H.rel_ "canonical", H.href_ $ "https://zoomhub.net" <> path]
      H.meta_ [H.name_ "viewport", H.content_ "width=device-width, initial-scale=1"]
      H.link_ [H.rel_ "shortcut icon", H.href_ "favicon.ico"]
      appleTouchIcons

      H.link_ [H.rel_ "stylesheet", H.type_ "text/css", H.href_ "https://rsms.me/inter/inter.css"]
      H.link_ [H.rel_ "stylesheet", H.type_ "text/css", H.href_ "/styles/global.css"]

      analyticsScript

      forM_ pageHeadStyles id
    H.body_
      [H.class_ (fromMaybe "h-full bg-black m-0 p-0" pageBodyClassName)]
      pageBody

--- TODO: Improve how we represent analytics code.
--- TODO: Pass through `G-*` Google Analytics measurement ID.
analyticsScript :: (Monad m) => H.HtmlT m ()
analyticsScript = do
  H.script_
    [ H.async_ "async",
      H.src_ ("https://www.googletagmanager.com/gtag/js?id=" <> measurementId)
    ]
    ("" :: Text)
  H.script_
    [text|
      window.dataLayer = window.dataLayer || [];
      function gtag(){dataLayer.push(arguments);}
      gtag('js', new Date());

      gtag('config', "$measurementId");
    |]
  where
    measurementId = Config.googleAnalyticsMeasurementId

appleTouchIcons :: (Monad m) => H.HtmlT m ()
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
