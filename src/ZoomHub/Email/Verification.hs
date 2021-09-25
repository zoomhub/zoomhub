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
    subject = "ZoomHub: View your upload"
    body =
      "Hi,\n\
      \\n\
      \Thanks for your upload to ZoomHub (formerly zoom.it).\n\
      \\n\
      \You can now view it at: "
        <> verificationURL
        <> "\n\n\
           \If you haven’t uploaded anything to ZoomHub, please ignore this email.\n\
           \\n\
           \Thanks,\n\
           \Daniel from ZoomHub\n\
           \\n\
           \\n\
           \P.S. If you have any questions or feedback, simply reply to this email. I read each and every message :)"
    verificationURL =
      T.pack (show baseURI)
        <> "/"
        <> T.pack (unContentId contentId)
        <> "/verify/"
        <> T.pack (show verificationToken)
