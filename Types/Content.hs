{-# LANGUAGE DeriveGeneric #-}
module Types.Content where


import Data.Aeson as Aeson
import Data.Aeson.Casing
import Types.DeepZoomImage

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
  , contentUrl="http://example.com/" ++ contentId ++ ".jpg"
  , contentReady=False
  , contentFailed=False
  , contentProgress=1.0
  , contentMime="image/jpeg"
  , contentSize=42000
  , contentActive=False
  , contentActiveAt="2015-02-23T04:23:29.754Z"
  , contentFinishedAt="2015-02-23T04:23:34.703Z"
  , contentDzi=Just DeepZoomImage
    { dziWidth=4013
    , dziHeight=2405
    , dziTileSize=254
    , dziTileOverlap=1
    , dziTileFormat="jpg"
    }
  }
