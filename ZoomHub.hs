{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
module ZoomHub where

import Data.Aeson
import Data.Aeson.Casing
import Data.Char
import GHC.Generics
import Network.Wai
import Servant


data DeepZoomImage = DeepZoomImage
  { dziWidth :: Integer
  , dziHeight :: Integer
  , dziTileSize :: Integer
  , dziTileOverlap :: Integer
  , dziTileFormat :: String
  } deriving (Eq, Show, Generic)

instance ToJSON DeepZoomImage where
   toJSON = genericToJSON $ aesonPrefix camelCase
instance FromJSON DeepZoomImage where
   parseJSON = genericParseJSON $ aesonPrefix camelCase

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
  } deriving (Eq, Show, Generic)

instance ToJSON Content where
   toJSON = genericToJSON $ aesonPrefix camelCase
instance FromJSON Content where
   parseJSON = genericParseJSON $ aesonPrefix camelCase

type ContentAPI =
  "v1" :> "content" :> Capture "id" String :> Get '[JSON] Content

content :: String -> Content
content contentId = Content
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

contentAPI :: Proxy ContentAPI
contentAPI = Proxy

server :: Server ContentAPI
server contentId = return $ content contentId

app :: Application
app = serve contentAPI server
