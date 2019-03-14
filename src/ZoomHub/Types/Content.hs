module ZoomHub.Types.Content
  ( Content(..)
  , mkContent
  ) where

import Data.Text (Text)
import Data.Time.Clock (UTCTime)

import ZoomHub.Types.ContentId (ContentId)
import ZoomHub.Types.ContentMIME (ContentMIME)
import ZoomHub.Types.ContentState (ContentState(Initialized))
import ZoomHub.Types.ContentType (ContentType)
import ZoomHub.Types.ContentURI (ContentURI)
import ZoomHub.Types.DeepZoomImage (DeepZoomImage)

-- Content
data Content = Content
  { contentId            :: ContentId
  , contentURL           :: ContentURI
  , contentType          :: ContentType
  , contentState         :: ContentState
  , contentInitializedAt :: UTCTime
  , contentActiveAt      :: Maybe UTCTime
  , contentCompletedAt   :: Maybe UTCTime
  , contentMIME          :: Maybe ContentMIME
  , contentSize          :: Maybe Integer
  , contentProgress      :: Float
  , contentNumViews      :: Integer
  , contentError         :: Maybe Text
  , contentDZI           :: Maybe DeepZoomImage
  } deriving (Eq, Show)

mkContent :: ContentType -> ContentId -> ContentURI -> UTCTime -> Content
mkContent type_ cId uri initializedAt = Content
  { contentId = cId
  , contentURL = uri
  , contentType = type_
  , contentState = Initialized
  , contentInitializedAt = initializedAt
  , contentActiveAt = Nothing
  , contentCompletedAt = Nothing
  , contentMIME = Nothing
  , contentSize = Nothing
  , contentProgress = 0.0
  , contentNumViews = 0
  , contentError = Nothing
  , contentDZI = Nothing
  }
