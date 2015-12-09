{-# LANGUAGE DeriveGeneric #-}

module ZoomHub.Types.Content where


import Data.Aeson as Aeson
import Data.Aeson.Casing
import ZoomHub.Types.DeepZoomImage

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
  , contentActiveAt :: String -- Use proper date type
  , contentFinishedAt :: String -- Use proper date type
  , contentDzi :: Maybe DeepZoomImage
  } deriving (Eq, Show, GHC.Generic)

instance Aeson.ToJSON Content where
   toJSON = genericToJSON $ aesonPrefix camelCase
instance Aeson.FromJSON Content where
   parseJSON = genericParseJSON $ aesonPrefix camelCase

-- Constructor: Content
mkContent :: String -> Content
mkContent contentId = Content
  { contentId=contentId
  , contentUrl="http://EXAMPLE.COM/" ++ contentId ++ ".JPG"
  , contentReady=False
  , contentFailed=False
  , contentProgress=1.0
  , contentMime="image/jpeg"
  , contentSize=42000
  , contentActive=False
  , contentActiveAt="1969-01-01T00:00:00.000Z"
  , contentFinishedAt="1969-01-01T00:00:00.000Z"
  , contentDzi=Just DeepZoomImage
    { dziWidth=1024
    , dziHeight=1024
    , dziTileSize=254
    , dziTileOverlap=1
    , dziTileFormat="jpg"
    }
  }
