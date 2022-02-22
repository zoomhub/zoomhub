{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
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
import qualified Lucid as H
import NeatInterpolation (text)
import Network.URI (parseRelativeReference, relativeTo)
import qualified Network.URI.Encode as URI
import ZoomHub.API.Types.Content (Content, contentId, contentShareUrl)
import ZoomHub.Types.BaseURI (BaseURI, unBaseURI)
import ZoomHub.Types.ContentId (unContentId)
import ZoomHub.Utils (tshow)
import ZoomHub.Web.Page (Page (Page), Path (..), Title (..))
import qualified ZoomHub.Web.Page as Page

data ViewContent = ViewContent
  { vcContent :: Content,
    vcBaseURI :: BaseURI
  }
  deriving (Eq, Show)

mkViewContent :: BaseURI -> Content -> ViewContent
mkViewContent vcBaseURI vcContent = ViewContent {..}

instance H.ToHtml ViewContent where
  toHtml vc =
    Page.layout $
      Page
        { pageTitle = Title $ T.pack cId <> " — " <> Page.title,
          pageCanonicalPath = Just $ Path $ "/" <> T.pack cId,
          pageBody = do
            H.script_ [H.src_ scriptURI] ("" :: Text)
            H.div_
              [H.class_ "fixed left-0 bottom-0 right-0 px-2 py-3 flex justify-center items-center text-sm text-white space-x-1"]
              do
                embedButton
                emailButton
                tweetButton
            H.script_
              [text|
                document.querySelector("#embed-button").onclick = async () => {
                  try {
                    await navigator.clipboard.writeText("$embedCode")
                  } catch (error) {
                    console.error(error)
                    alert("Sorry, couldn’t copy embed code to clipboard.")
                    return
                  }

                  alert("🔥 Success! Copied embed code to clipboard.")
                }
              |]
        }
    where
      content = vcContent vc
      cId = unContentId $ contentId content
      baseURI = vcBaseURI vc

      scriptBaseURI = tshow $ scriptPath `relativeTo` unBaseURI baseURI
      scriptPath = fromJust . parseRelativeReference $ cId <> ".js"
      scriptURI =
        scriptBaseURI
          <> T.pack
            ( fold
                [ "?",
                  "id=container",
                  "&",
                  "width=" <> URI.encode "100%",
                  "&",
                  "height=" <> URI.encode "100%"
                ]
            )

      embedCode =
        [text|
          <script src=\"$scriptBaseURI?width=535px&height=auto&border=none\"><\/script>
        |]

      embedButton =
        H.button_
          [ H.class_ "btn btn-sm btn-secondary bg-white text-gray-800 hover:bg-gray-800 hover:text-white",
            H.type_ "button",
            H.id_ "embed-button"
          ]
          do
            H.span_ [H.class_ "mr-1"] $
              H.toHtmlRaw
                [text|
                  <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="feather feather-code"><polyline points="16 18 22 12 16 6"></polyline><polyline points="8 6 2 12 8 18"></polyline></svg>
                |]
            H.toHtml ("Embed" :: Text)

      emailButton =
        H.a_
          [ H.class_ "btn btn-sm btn-secondary bg-gray-500 hover:bg-gray-600 text-white",
            H.target_ "_blank",
            H.href_ $
              fold
                [ "mailto:",
                  "?subject=" <> URI.encodeText ("ZoomHub: " <> T.pack cId),
                  "&body=" <> (URI.encodeText . tshow $ contentShareUrl content)
                ]
          ]
          do
            H.span_ [H.class_ "mr-1"] $
              H.toHtmlRaw
                [text|
                  <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="feather feather-mail"><path d="M4 4h16c1.1 0 2 .9 2 2v12c0 1.1-.9 2-2 2H4c-1.1 0-2-.9-2-2V6c0-1.1.9-2 2-2z"></path><polyline points="22,6 12,13 2,6"></polyline></svg>
                |]
            H.toHtml ("Email" :: Text)

      tweetButton =
        H.a_
          [ H.class_ "btn btn-sm btn-secondary bg-blue-500 hover:bg-blue-600 text-white",
            H.target_ "_blank",
            H.href_
              ( fold
                  [ "https://twitter.com/intent/tweet",
                    "?",
                    "text=ZoomHub:%20" <> T.pack cId,
                    "&",
                    "url=" <> tshow (contentShareUrl content) <> "&via=ZoomHub&related=ZoomHub,gasi"
                  ]
              )
          ]
          do
            H.span_ [H.class_ "mr-1"] $
              H.toHtmlRaw
                [text|
                  <svg xmlns="http://www.w3.org/2000/svg" width="16" height="16" viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" class="feather feather-twitter"><path d="M23 3a10.9 10.9 0 0 1-3.14 1.53 4.48 4.48 0 0 0-7.86 3v1A10.66 10.66 0 0 1 3 4s-4 9 5 13a11.64 11.64 0 0 1-7 2c9 5 20 0 20-11.5a4.5 4.5 0 0 0-.08-.83A7.72 7.72 0 0 0 23 3z"></path></svg>
                |]
            H.toHtml ("Tweet" :: Text)

  toHtmlRaw = H.toHtml
