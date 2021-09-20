{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Email.Verification
  ( request,
  )
where

import qualified Data.Text as T
import ZoomHub.Email (Email (..), From, To)
import ZoomHub.Types.BaseURI (BaseURI)
import ZoomHub.Types.ContentId (ContentId, unContentId)
import ZoomHub.Types.VerificationToken (VerificationToken)

-- TODO: Distinguish between upload and URL submission:
request :: BaseURI -> ContentId -> VerificationToken -> From -> To -> Email
request baseURI contentId verificationToken from to =
  Email {from, to, subject, body}
  where
    subject = "ZoomHub: View your submission"
    body =
      "Hi,\n\
      \\n\
      \Thanks for your submission to ZoomHub (formerly zoom.it).\n\
      \\n\
      \To view it, please follow this link:\n"
        <> verificationURL
        <> "\n\nIf you haven’t submitted anything to ZoomHub, just ignore this email.\n\n\
           \Thanks,\n\
           \Daniel from ZoomHub\n"
    verificationURL =
      T.pack (show baseURI)
        <> "/v1/content/"
        <> T.pack (unContentId contentId)
        <> "/verification/"
        <> T.pack (show verificationToken)
