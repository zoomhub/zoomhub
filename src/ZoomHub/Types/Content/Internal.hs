{-# LANGUAGE DeriveGeneric #-}

module ZoomHub.Types.Content.Internal where

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
import ZoomHub.Types.ContentId (ContentId)
import ZoomHub.Types.ContentMIME (ContentMIME)
import ZoomHub.Types.ContentState (ContentState (Initialized))
import ZoomHub.Types.ContentType (ContentType)
import ZoomHub.Types.ContentURI (ContentURI)
import ZoomHub.Types.DeepZoomImage (DeepZoomImage)
import ZoomHub.Types.VerificationToken (VerificationToken)

-- Content
data Content = Content
  { contentId :: ContentId,
    contentType :: ContentType,
    contentURL :: ContentURI,
    contentState :: ContentState,
    contentInitializedAt :: UTCTime,
    contentActiveAt :: Maybe UTCTime,
    contentCompletedAt :: Maybe UTCTime,
    contentMIME :: Maybe ContentMIME,
    contentSize :: Maybe Int64,
    contentProgress :: Double,
    contentNumViews :: Int64,
    contentError :: Maybe Text,
    contentDZI :: Maybe DeepZoomImage,
    contentSubmitterEmail :: Maybe Text, -- TODO: Introduce `Email` type
    contentVerificationToken :: Maybe VerificationToken,
    contentVerifiedAt :: Maybe UTCTime,
    contentUserId :: Maybe Int64
  }
  deriving (Eq, GHC.Generic, Show)

mkContent :: ContentType -> ContentId -> ContentURI -> UTCTime -> Content
mkContent type_ cId uri initializedAt =
  Content
    { contentId = cId,
      contentType = type_,
      contentURL = uri,
      contentState = Initialized,
      contentInitializedAt = initializedAt,
      contentActiveAt = Nothing,
      contentCompletedAt = Nothing,
      contentMIME = Nothing,
      contentSize = Nothing,
      contentProgress = 0.0,
      contentNumViews = 0,
      contentError = Nothing,
      contentDZI = Nothing,
      contentSubmitterEmail = Nothing,
      contentVerificationToken = Nothing,
      contentVerifiedAt = Nothing,
      contentUserId = Nothing
    }

-- PostgreSQL / Squeal
instance SOP.Generic Content

instance SOP.HasDatatypeInfo Content
