module ZoomHub.Types.Content
  ( Content(..)
  , mkContent
  ) where

import           Data.Time.Clock             (UTCTime)

import           ZoomHub.Types.ContentId     (ContentId)
import           ZoomHub.Types.ContentState  (ContentState (Initialized))
import           ZoomHub.Types.ContentURI    (ContentURI)
import           ZoomHub.Types.DeepZoomImage (DeepZoomImage)

-- Content
data Content = Content
  { contentId            :: ContentId
  , contentURL           :: ContentURI
  , contentState         :: ContentState
  , contentInitializedAt :: Maybe UTCTime
  , contentActiveAt      :: Maybe UTCTime
  , contentCompletedAt   :: Maybe UTCTime
  , contentMIME          :: Maybe String -- TODO: Use proper MIME type
  , contentSize          :: Maybe Integer
  , contentProgress      :: Float
  , contentDZI           :: Maybe DeepZoomImage
  } deriving (Eq, Show)

mkContent :: ContentId -> ContentURI -> Content
mkContent cId uri = Content
  { contentId = cId
  , contentURL = uri
  , contentState = Initialized
  , contentInitializedAt = Nothing
  , contentActiveAt = Nothing
  , contentCompletedAt = Nothing
  , contentMIME = Nothing
  , contentSize = Nothing
  , contentProgress = 0.0
  , contentDZI = Nothing
  }
