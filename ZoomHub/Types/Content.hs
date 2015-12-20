{-# LANGUAGE DeriveGeneric #-}

module ZoomHub.Types.Content
  ( ContentId(ContentId)
  , Content
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
  ) where


import Data.Aeson as Aeson
import Data.Aeson.Casing
import ZoomHub.Types.DeepZoomImage

import qualified Data.Text as T
import qualified Data.Time.Clock as DTC
import qualified GHC.Generics as GHC
import qualified Servant as S


-- ContentId
newtype ContentId = ContentId String
  deriving (Eq, GHC.Generic)

instance Show ContentId where
  show (ContentId cid) = cid

instance S.FromText ContentId where
  fromText t = Just $ ContentId $ T.unpack t

instance Aeson.ToJSON ContentId where
   toJSON = genericToJSON $ aesonPrefix camelCase
instance Aeson.FromJSON ContentId where
   parseJSON = genericParseJSON $ aesonPrefix camelCase

-- Content
data Content = Content
  { contentId :: ContentId
  , contentUrl :: String
  , contentReady :: Bool
  , contentFailed :: Bool
  , contentProgress :: Float
  , contentMime :: Maybe String -- Use proper MIME type
  , contentSize :: Maybe Integer
  , contentActive :: Bool
  , contentActiveAt :: Maybe DTC.UTCTime
  , contentFinishedAt :: Maybe DTC.UTCTime
  , contentDzi :: Maybe DeepZoomImage
  } deriving (Eq, Show, GHC.Generic)

instance Aeson.ToJSON Content where
   toJSON = genericToJSON $ aesonPrefix camelCase
instance Aeson.FromJSON Content where
   parseJSON = genericParseJSON $ aesonPrefix camelCase

-- Constructor
mkContent :: ContentId -> String -> Content
mkContent cid url = Content
  { contentId = cid
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
