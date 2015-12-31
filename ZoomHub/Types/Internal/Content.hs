{-# LANGUAGE DeriveGeneric #-}
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
  , mkContent
  , prettyEncodeConfig
  ) where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Casing as AC
import qualified Data.Aeson.Encode.Pretty as AP
import qualified Data.Ord as Ord
import qualified Data.Text as T
import qualified Data.Time.Clock as DTC
import qualified GHC.Generics as GHC
import qualified ZoomHub.Types.Internal.ContentId as IC
import qualified ZoomHub.Types.Internal.DeepZoomImage as ID

-- Content
data Content = Content
  { contentId :: IC.ContentId
  , contentUrl :: String
  , contentReady :: Bool
  , contentFailed :: Bool
  , contentProgress :: Float
  , contentMime :: Maybe String -- Use proper MIME type
  , contentSize :: Maybe Integer
  , contentActive :: Bool
  , contentActiveAt :: Maybe DTC.UTCTime
  , contentFinishedAt :: Maybe DTC.UTCTime
  , contentDzi :: Maybe ID.DeepZoomImage
  } deriving (Eq, Show, GHC.Generic)

instance Aeson.ToJSON Content where
   toJSON = Aeson.genericToJSON $ AC.aesonPrefix AC.camelCase
instance Aeson.FromJSON Content where
   parseJSON = Aeson.genericParseJSON $ AC.aesonPrefix AC.camelCase

-- Constructor
mkContent :: IC.ContentId -> String -> Content
mkContent cId url = Content
  { contentId = cId
  , contentUrl = url
  , contentReady = False
  , contentFailed = False
  , contentProgress = 0.0
  , contentMime = Nothing
  , contentSize = Nothing
  , contentActive = False
  , contentActiveAt = Nothing
  , contentFinishedAt = Nothing
  , contentDzi = Nothing
  }

prettyEncodeConfig :: AP.Config
prettyEncodeConfig = AP.Config
  { AP.confIndent = 2
  , AP.confCompare = keyCompare
  }

keyCompare :: T.Text -> T.Text -> Ordering
keyCompare = AP.keyOrder keyOrder `mappend` Ord.comparing T.length

keyOrder :: [T.Text]
keyOrder =
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
  , "dzi"
  ]
