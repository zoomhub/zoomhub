{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module ZoomHub where

import Data.Aeson
import GHC.Generics
import Network.Wai
import Servant

data Content = Content
  { contentId :: String
  , url :: String
  , ready :: Bool
  , failed :: Bool
  , progress :: Float
  , mime :: String -- Use proper MIME type
  , size :: Integer
  , active :: Bool
  , activeAt :: String -- Use proper date type
  , finishedAt :: String -- Use proper date type
  , dzi :: Maybe DeepZoomImage
  } deriving (Eq, Show, Generic)

instance ToJSON Content

data DeepZoomImage = DeepZoomImage
  { width :: Integer
  , height :: Integer
  , tileSize :: Integer
  , tileOverlap :: Integer
  , tileFormat :: String
  } deriving (Eq, Show, Generic)

instance ToJSON DeepZoomImage

type ContentAPI =
  "v1" :> "content" :> Capture "id" String :> Get '[JSON] Content

content :: String -> Content
content contentId = Content
  { contentId=contentId
  , url="http://example.com/" ++ contentId ++ ".jpg"
  , ready=False
  , failed=False
  , progress=1.0
  , mime="image/jpeg"
  , size=42000
  , active=False
  , activeAt="2015-02-23T04:23:29.754Z"
  , finishedAt="2015-02-23T04:23:34.703Z"
  , dzi=Just DeepZoomImage
    { width=4013
    , height=2405
    , tileSize=254
    , tileOverlap=1
    , tileFormat="jpg"
    }
  }

contentAPI :: Proxy ContentAPI
contentAPI = Proxy

server :: Server ContentAPI
server contentId = return $ content contentId

app :: Application
app = serve contentAPI server
