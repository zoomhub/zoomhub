{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.Content
  ( Content
  , contentId
  , contentUrl
  , contentReady
  , contentFailed
  , contentProgress
  , contentDzi
  , fromInternal
  ) where

import           Data.Aeson                       (FromJSON, ToJSON,
                                                   genericParseJSON,
                                                   genericToJSON, parseJSON,
                                                   toJSON)
import           Data.Aeson.Casing                (aesonPrefix, camelCase)
import           Data.List                        (intersperse)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import           Lucid                            (ToHtml, body_, content_,
                                                   doctypehtml_, head_, meta_,
                                                   name_, script_, src_, style_,
                                                   title_, toHtml, toHtmlRaw)

import           ZoomHub.Types.DeepZoomImage      (DeepZoomImage)
import qualified ZoomHub.Types.DeepZoomImage      as DZ
import qualified ZoomHub.Types.Internal.Content   as Internal
import           ZoomHub.Types.Internal.ContentId (ContentId, unId)


-- Content
data Content = Content
  { contentId        :: ContentId
  , contentUrl       :: String
  , contentReady     :: Bool
  , contentFailed    :: Bool
  , contentProgress  :: Float
  , contentShareUrl  :: String
  , contentEmbedHtml :: String
  , contentDzi       :: Maybe DeepZoomImage
  } deriving (Eq, Show, Generic)

-- Constructor
fromInternal :: Internal.Content -> Content
fromInternal c = Content
  { contentId = cId
  , contentUrl = Internal.contentUrl c
  , contentReady = Internal.contentReady c
  , contentFailed = Internal.contentFailed c
  , contentProgress = Internal.contentProgress c
  , contentShareUrl = shareURL
  , contentEmbedHtml = embedHtml
  , contentDzi = dzi
  }
  where
    cId = Internal.contentId c
    -- TODO: Make hostname dynamic:
    shareURL = "http://zoom.it/" ++ unId cId
    embedHtml =
      "<script src=\"" ++ shareURL ++ ".js?width=auto&height=400px\"></script>"
    dzi = DZ.fromInternal cId <$> Internal.contentDzi c

-- JSON
instance ToJSON Content where
   toJSON = genericToJSON $ aesonPrefix camelCase
instance FromJSON Content where
   parseJSON = genericParseJSON $ aesonPrefix camelCase


-- HTML
concatPretty :: [T.Text] -> T.Text
concatPretty = T.concat . intersperse "\n"

-- TODO: Improve how we represent inline styles.
styles :: T.Text
styles = concatPretty [
    "html, body {",
    "  background-color: #000;",
    "  color: #fff;",
    "  font-family: \"Helvetica Neue\", Helvetica, Arial, sans-serif;",
    "  height: 100%;",
    "  margin: 0;",
    "  padding: 0;",
    "}",
    "",
    ".__seadragon {",
    "  height: 100% !important;",
    "}"
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

-- TODO: Pass through host:
instance ToHtml Content where
  toHtml content =
      doctypehtml_ $
        do head_ (do title_ (toHtml $ cId <> " — zoom.it")
                     meta_ [name_ "viewport",
                            content_ "width=device-width, initial-scale=1"]
                     style_ styles
                     script_ analyticsScript
                     )
           body_ $ script_ [src_ scriptURL] ("" :: T.Text)
    where
      scriptURL = "http://zoom.it/" <> cId <> ".js?width=auto&height=400px"
      cId = T.pack . unId $ contentId content
  toHtmlRaw = toHtml
