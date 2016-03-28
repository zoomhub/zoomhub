module ZoomHub.Types.Content
  ( Content(Content)
  , contentId
  , contentUrl
  , contentState
  , contentInitializedAt
  , contentActiveAt
  , contentCompletedAt
  , contentMime
  , contentSize
  , contentProgress
  , contentDzi
  ) where

import           Data.Time.Clock             (UTCTime)

import           ZoomHub.Types.ContentId     (ContentId)
import           ZoomHub.Types.ContentState  (ContentState)
import           ZoomHub.Types.ContentURI    (ContentURI)
import           ZoomHub.Types.DeepZoomImage (DeepZoomImage)

-- Content
data Content = Content
  { contentId            :: ContentId
  , contentUrl           :: ContentURI
  , contentState         :: ContentState
  , contentInitializedAt :: Maybe UTCTime
  , contentActiveAt      :: Maybe UTCTime
  , contentCompletedAt   :: Maybe UTCTime
  , contentMime          :: Maybe String -- TODO: Use proper MIME type
  , contentSize          :: Maybe Integer
  , contentProgress      :: Float
  , contentDzi           :: Maybe DeepZoomImage
  } deriving (Eq, Show)
