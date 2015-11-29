{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module ZoomHub where

import Data.Aeson
import GHC.Generics
import Network.Wai
import Servant

data DeepZoomImage = DeepZoomImage
  { url :: String
  , width :: Integer
  , height :: Integer
  , tileSize :: Integer
  , tileOverlap :: Integer
  , tileFormat :: String
  } deriving (Eq, Show, Generic)

instance ToJSON DeepZoomImage

type ContentAPI = "content" :> Get '[JSON] DeepZoomImage

content :: DeepZoomImage
content =
  DeepZoomImage "http://content.zoomhub.net/dzis/h.dzi" 4013 2405 254 1 "jpg"

contentAPI :: Proxy ContentAPI
contentAPI = Proxy

server :: Server ContentAPI
server = return content

app :: Application
app = serve contentAPI server

