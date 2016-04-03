module ZoomHub.Types.Content
  ( Content(..)
  , mkContent
  ) where

import           Data.Time.Clock             (UTCTime)

import           ZoomHub.Types.ContentId     (ContentId)
import           ZoomHub.Types.ContentMIME   (ContentMIME)
import           ZoomHub.Types.ContentState  (ContentState (Initialized))
import           ZoomHub.Types.ContentURI    (ContentURI)
import           ZoomHub.Types.DeepZoomImage (DeepZoomImage)

-- Content
data Content = Content
  { contentId            :: ContentId
  , contentURL           :: ContentURI
  , contentState         :: ContentState
  , contentInitializedAt :: UTCTime
  , contentActiveAt      :: Maybe UTCTime
  , contentCompletedAt   :: Maybe UTCTime
  , contentMIME          :: Maybe ContentMIME
  , contentSize          :: Maybe Integer
  , contentProgress      :: Float
  , contentDZI           :: Maybe DeepZoomImage
  } deriving (Eq, Show)

mkContent :: ContentId -> ContentURI -> UTCTime -> Content
mkContent cId uri initializedAt = Content
  { contentId = cId
  , contentURL = uri
  , contentState = Initialized
  , contentInitializedAt = initializedAt
  , contentActiveAt = Nothing
  , contentCompletedAt = Nothing
  , contentMIME = Nothing
  , contentSize = Nothing
  , contentProgress = 0.0
  , contentDZI = Nothing
  }
