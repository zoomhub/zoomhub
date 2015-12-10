{-# LANGUAGE DeriveGeneric #-}

module ZoomHub.Types.Content where


import Data.Aeson as Aeson
import Data.Aeson.Casing
import ZoomHub.Types.DeepZoomImage

import qualified Data.Time.Clock as DTC
import qualified GHC.Generics as GHC


data ContentState = Inactive | Active | Failed | Ready

data Content = Content
  { contentId :: String
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
