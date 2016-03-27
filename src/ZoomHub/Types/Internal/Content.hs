{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.Internal.Content
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

import           Data.Time.Clock                      (UTCTime)

import           ZoomHub.Types.Internal.ContentId     (ContentId)
import           ZoomHub.Types.Internal.ContentState  (ContentState)
import           ZoomHub.Types.Internal.ContentURI    (ContentURI)
import           ZoomHub.Types.Internal.DeepZoomImage (DeepZoomImage)

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
