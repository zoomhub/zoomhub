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

type ContentAPI =
    "v1" :> "content" :> Capture "id" String :> Get '[JSON] DeepZoomImage

content :: String -> DeepZoomImage
content id =
  DeepZoomImage ("http://content.zoomhub.net/dzis/" ++ id ++ ".dzi") 4013 2405 254 1 "jpg"

contentAPI :: Proxy ContentAPI
contentAPI = Proxy

server :: Server ContentAPI
server id = return $ content id

app :: Application
app = serve contentAPI server
