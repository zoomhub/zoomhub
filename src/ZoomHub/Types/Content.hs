{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.Content
  ( Content
  , ContentShareURI
  , contentDzi
  , contentFailed
  , contentId
  , contentProgress
  , contentReady
  , contentUrl
  , fromInternal
  , unContentShareURI
  ) where

import           Data.Aeson                       (ToJSON, Value (String),
                                                   genericToJSON, toJSON)
import           Data.Aeson.Casing                (aesonPrefix, camelCase)
import           Data.Maybe                       (fromJust)
import           Data.Monoid                      ((<>))
import qualified Data.Text                        as T
import           GHC.Generics                     (Generic)
import           Lucid                            (ToHtml, body_, content_,
                                                   doctypehtml_, head_, meta_,
                                                   name_, script_, src_, style_,
                                                   title_, toHtml, toHtmlRaw)
import           Network.URI                      (URI, parseRelativeReference,
                                                   relativeTo)

import           ZoomHub.Types.BaseURI            (BaseURI, unBaseURI)
import           ZoomHub.Types.ContentBaseURI     (ContentBaseURI)
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
  , contentShareUrl  :: ContentShareURI
  , contentEmbedHtml :: String
  , contentDzi       :: Maybe DeepZoomImage
  } deriving (Eq, Show, Generic)

-- Constructor
fromInternal :: BaseURI -> ContentBaseURI -> Internal.Content -> Content
fromInternal baseURI contentBaseURI c = Content
  { contentId = cId
  , contentUrl = Internal.contentUrl c
  , contentReady = Internal.contentReady c
  , contentFailed = Internal.contentFailed c
  , contentProgress = Internal.contentProgress c
  , contentShareUrl = shareURI
  , contentEmbedHtml = embedHTML
  , contentDzi = dzi
  }
  where
    cId = Internal.contentId c
    shareURI = ContentShareURI $ sharePathURI `relativeTo` unBaseURI baseURI
    sharePathURI = fromJust . parseRelativeReference $ unId cId
    scriptSource = show shareURI ++ ".js?width=auto&height=400px"
    embedHTML = "<script src=\"" ++ scriptSource ++ "\"></script>"
    dzi = DZ.fromInternal contentBaseURI cId <$> Internal.contentDzi c

-- JSON
instance ToJSON Content where
   toJSON = genericToJSON $ aesonPrefix camelCase

-- HTML
concatPretty :: [T.Text] -> T.Text
concatPretty = T.intercalate "\n"

-- TODO: Improve how we represent inline styles.
styles :: T.Text
styles = concatPretty
  [ "html, body {"
  , "  background-color: #000;"
  , "  color: #fff;"
  , "  font-family: \"Helvetica Neue\", Helvetica, Arial, sans-serif;"
  , "  height: 100%;"
  , "  margin: 0;"
  , "  padding: 0;"
  , "}"
  , ""
  , ".__zoomhub {"
  , "  height: 100% !important;"
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
           body_ $ script_ [src_ scriptURL] emptyBody
    where
      emptyBody :: T.Text
      emptyBody = ""

      scriptURL = "/" <> cId <> ".js?width=auto&height=100%"
      cId = T.pack . unId $ contentId content

  toHtmlRaw = toHtml

-- Types
newtype ContentShareURI = ContentShareURI { unContentShareURI :: URI }
  deriving Eq

instance Show ContentShareURI where
  show = show . unContentShareURI

instance ToJSON ContentShareURI where
  toJSON = String . T.pack . show . unContentShareURI
