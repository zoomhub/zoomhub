{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}

module ZoomHub.Web.Page.VerifyContent
  ( VerifyContent,
    mkVerifyContent,
    VerificationResult (..),
  )
where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Lucid (ToHtml, toHtml, toHtmlRaw)
import qualified Lucid as H
import NeatInterpolation (text)
import ZoomHub.API.Types.Content (Content, contentId, contentShareUrl)
import ZoomHub.Types.BaseURI (BaseURI)
import ZoomHub.Types.ContentId (ContentId, unContentId)
import qualified ZoomHub.Web.Page as Page

data VerifyContent = VerifyContent
  { vcResult :: VerificationResult,
    vcBaseURI :: BaseURI
  }
  deriving (Eq, Show)

data VerificationResult
  = Success Content
  | Error Text
  deriving (Eq, Show)

mkVerifyContent :: BaseURI -> VerificationResult -> VerifyContent
mkVerifyContent vcBaseURI vcResult = VerifyContent {..}

progressScript :: ContentId -> Text
progressScript cId =
  [text|
    ;(() => {
      setInterval(async () => {
        const content = await fetch("$apiURL").then(
          (response) => response.json()
        )

        console.log("---")
        console.log(content)

        if (content.ready) {
          location.href = content.shareUrl
        }
      }, 2000)
    })()
  |]
  where
    apiURL = "/v1/content/" <> T.pack (unContentId cId)

instance ToHtml VerifyContent where
  toHtml (VerifyContent {..}) =
    Page.layout Page.title do
      H.div_
        [ H.style_ $
            T.intercalate
              ";"
              [ "display: flex",
                "flex-flow: column",
                "align-items: center",
                "justify-content: center",
                "height: 100%",
                "text-align: center"
              ]
        ]
        $ case vcResult of
          Success content -> do
            H.script_ $
              progressScript (contentId content)
            H.h1_ [H.class_ "title"] "🔧 Your upload is now being processed…"
            H.p_
              [H.style_ "color: #fff;"]
              do
                "🕐 Hang in there, this may take up to a few minutes."
                H.br_ []
                "➡️ We’ll redirect you as soon as it’s ready."
            H.p_
              [H.style_ "color: #fff;"]
              "\x1F971 If it’s taking too long, you can also close this page and come back via:"
            H.a_
              [H.href_ (T.pack . show $ contentShareUrl content)]
              (toHtml . show $ contentShareUrl content)
          Error message -> do
            H.h2_
              [H.style_ "color: #fff;"]
              "Oops, something went wrong"
            H.p_
              [H.style_ "color: #fff;"]
              (H.toHtml message)

  toHtmlRaw = toHtml
