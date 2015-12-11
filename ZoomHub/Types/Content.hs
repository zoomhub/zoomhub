{-# LANGUAGE DeriveGeneric #-}

module ZoomHub.Types.Content where


import Data.Aeson as Aeson
import Data.Aeson.Casing
import ZoomHub.Types.DeepZoomImage

import qualified Data.Text as T
import qualified Data.Time.Clock as DTC
import qualified GHC.Generics as GHC
import qualified Servant as S


data ContentState = Inactive | Active | Failed | Ready
data ContentMIME = JPEG | GIF | PNG | PDF | HTML

-- ContentId
newtype ContentId = ContentId String
  deriving (Eq, GHC.Generic)

instance Show ContentId where
  show (ContentId contentId) = contentId

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
  , contentMime :: String -- Use proper MIME type
  , contentSize :: Integer
  , contentActive :: Bool
  , contentActiveAt :: DTC.UTCTime
  , contentFinishedAt :: DTC.UTCTime
  , contentDzi :: Maybe DeepZoomImage
  } deriving (Eq, Show, GHC.Generic)

instance Aeson.ToJSON Content where
   toJSON = genericToJSON $ aesonPrefix camelCase
instance Aeson.FromJSON Content where
   parseJSON = genericParseJSON $ aesonPrefix camelCase
