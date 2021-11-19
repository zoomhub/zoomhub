{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ZoomHub.Email.Verification
  ( request,
  )
where

import qualified Data.Text as T
import NeatInterpolation (text)
import ZoomHub.Email (Email (..), From, To)
import ZoomHub.Types.BaseURI (BaseURI)
import ZoomHub.Types.ContentId (ContentId, unContentId)
import ZoomHub.Types.VerificationToken (VerificationToken)

-- TODO: Distinguish between upload and URL submission:
request :: BaseURI -> ContentId -> VerificationToken -> From -> To -> Email
request baseURI contentId verificationToken from to =
  Email {from, to, subject, body}
  where
    subject = "View your upload"
    body =
      [text|
        Hi,

        Thanks for your upload to ZoomHub (formerly zoom.it).

        Verify your email to view your upload: $verificationURL

        Helpful links
        - Share link: $wwwURL
        - How to embed your image: $baseURL/#embed
        - Developer API: $baseURL/#api

        If you haven’t uploaded anything to ZoomHub, please ignore this email.

        Thanks,
        Daniel from ZoomHub


        P.S. Do you have a question or feedback? Simply reply to this email. I read each and every message :)
      |]

    baseURL = T.pack (show baseURI)
    wwwURL = baseURL <> "/" <> T.pack (unContentId contentId)
    verificationURL = wwwURL <> "/verify/" <> T.pack (show verificationToken)
