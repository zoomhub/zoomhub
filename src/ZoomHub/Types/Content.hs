module ZoomHub.Types.Content
  ( Content,
    contentId,
    contentType,
    contentURL,
    contentState,
    contentInitializedAt,
    contentActiveAt,
    contentCompletedAt,
    contentMIME,
    contentSize,
    contentProgress,
    contentNumViews,
    contentError,
    contentDZI,
    contentSubmitterEmail,
    contentVerificationToken,
    contentVerifiedAt,
    contentUserId,
    version,
  )
where

import Data.Int (Int32)
import ZoomHub.Types.Content.Internal
  ( Content,
    contentActiveAt,
    contentCompletedAt,
    contentDZI,
    contentError,
    contentId,
    contentInitializedAt,
    contentMIME,
    contentNumViews,
    contentProgress,
    contentSize,
    contentState,
    contentSubmitterEmail,
    contentType,
    contentURL,
    contentUserId,
    contentVerificationToken,
    contentVerifiedAt,
  )

-- Content versions
--
-- Version 3: From 2009-09-10 until 2016-04-04
--
-- Version 4: From 2009-11-14 until 2014-09-03
--
-- Version 5: From 2021-09-16
-- - Introduces
--   - `submitter_email`
--   - `verification_token`
--   - `verified_at`
--   for tracking the author of a submission or upload.
version :: Int32
version = 5
