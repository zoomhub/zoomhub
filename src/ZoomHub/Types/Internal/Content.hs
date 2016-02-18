{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.Internal.Content
  ( Content
  , contentId
  , contentUrl
  , contentReady
  , contentFailed
  , contentProgress
  , contentMime
  , contentSize
  , contentActive
  , contentActiveAt
  , contentFinishedAt
  , contentDzi
  , contentRawPath
  , fromURL
  , prettyEncodeConfig
  ) where

import           Data.Aeson                           (FromJSON, ToJSON,
                                                       genericParseJSON,
                                                       genericToJSON, parseJSON,
                                                       toJSON)
import           Data.Aeson.Casing                    (aesonPrefix, camelCase)
import           Data.Aeson.Encode.Pretty             (Config (Config),
                                                       confCompare, confIndent,
                                                       keyOrder)
import           Data.Ord                             (comparing)
import qualified Data.Text                            as T
import           Data.Time.Clock                      (UTCTime)
import           GHC.Generics                         (Generic)

import           ZoomHub.Types.Internal.ContentId     (ContentId)
import           ZoomHub.Types.Internal.DeepZoomImage (DeepZoomImage)

-- Content
data Content = Content
  { contentId         :: ContentId
  , contentUrl        :: String
  , contentReady      :: Bool
  , contentFailed     :: Bool
  , contentProgress   :: Float
  , contentMime       :: Maybe String -- TODO: Use proper MIME type
  , contentSize       :: Maybe Integer
  , contentActive     :: Maybe Bool
  , contentActiveAt   :: Maybe UTCTime
  , contentFinishedAt :: Maybe UTCTime
  , contentDzi        :: Maybe DeepZoomImage
  , contentRawPath    :: Maybe FilePath
  } deriving (Eq, Show, Generic)

-- Constructor
fromURL :: ContentId -> String -> Content
fromURL cId url = Content
  { contentId = cId
  , contentUrl = url
  , contentReady = False
  , contentFailed = False
  , contentProgress = 0.0
  , contentMime = Nothing
  , contentSize = Nothing
  , contentActive = Just False
  , contentActiveAt = Nothing
  , contentFinishedAt = Nothing
  , contentDzi = Nothing
  , contentRawPath = Nothing
    }

-- JSON
instance ToJSON Content where
   toJSON = genericToJSON $ aesonPrefix camelCase
instance FromJSON Content where
   parseJSON = genericParseJSON $ aesonPrefix camelCase

prettyEncodeConfig :: Config
prettyEncodeConfig = Config
  { confIndent = 2
  , confCompare = keyCompare
  }

keyCompare :: T.Text -> T.Text -> Ordering
keyCompare = keyOrder keyOrdering `mappend` comparing T.length

keyOrdering :: [T.Text]
keyOrdering =
  [ "id"
  , "url"
  , "ready"
  , "failed"
  , "progress"
  , "mime"
  , "size"
  , "active"
  , "activeAt"
  , "finishedAt"
  , "rawPath"
  , "dzi"
  ]
