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

import Data.Text (Text)
import qualified Data.Text as T
import qualified Lucid as H
import NeatInterpolation (text)
import ZoomHub.API.Types.Content (Content, contentId, contentShareUrl)
import ZoomHub.Types.BaseURI (BaseURI)
import ZoomHub.Types.ContentId (ContentId, unContentId)
import ZoomHub.Web.Page (Page (Page), Title (Title))
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

instance H.ToHtml VerifyContent where
  toHtml VerifyContent {..} =
    Page.layout $
      Page
        { pageTitle = Title Page.title,
          pageCanonicalPath = Nothing,
          pageHeadStyles = Nothing,
          pageBodyClassName = Nothing,
          pageBody =
            H.div_
              [H.class_ "h-screen flex flex-col items-center justify-center"]
              $ H.div_ [H.class_ "p-8 md:p-0 space-y-4"]
              $ case vcResult of
                Success content -> do
                  H.script_ $
                    progressScript (contentId content)
                  H.h1_
                    [H.class_ "text-2xl lg:text-3xl text-white font-semibold tracking-tighter"]
                    "🔧 Your upload is now being processed…"
                  H.p_
                    [H.class_ "text-lg text-white"]
                    do
                      "🕐 Hang in there, this may take up to a few minutes."
                      H.br_ []
                      "➡️ We’ll redirect you as soon as it’s ready."
                  H.p_
                    [H.class_ "text-lg text-white"]
                    do
                      H.span_ "\x1F971 If it’s taking too long, you can also close this page and come back via: "
                      H.br_ []
                      H.a_
                        [ H.class_ "link",
                          H.href_ (T.pack . show $ contentShareUrl content)
                        ]
                        (H.toHtml . show $ contentShareUrl content)
                Error message -> do
                  H.h2_
                    [H.style_ "color: #fff;"]
                    "Oops, something went wrong"
                  H.p_
                    [H.style_ "color: #fff;"]
                    (H.toHtml message)
        }
  toHtmlRaw = H.toHtml
