{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
module ZoomHub where

import Control.Monad.Trans.Either
import Data.Aeson
import Data.Aeson.Casing
import Data.Char
import GHC.Generics
import Network.Wai
import Servant


-- Models
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
  } deriving (Eq, Show, Generic)

instance ToJSON Content where
   toJSON = genericToJSON $ aesonPrefix camelCase
instance FromJSON Content where
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

-- API
type API = "v1" :> "content" :> Capture "id" String :> Get '[JSON] Content
      :<|> "v1" :> "content" :> QueryParam "url" String :> Get '[JSON] Content

api :: Proxy API
api = Proxy

server :: Server API
server = contentById
    :<|> contentByURL

  where contentById :: String -> EitherT ServantErr IO Content
        contentById id = return $ mkContent id

        contentByURL :: Maybe String -> EitherT ServantErr IO Content
        contentByURL url = case url of
          Nothing  -> return . mkContent $ "404" -- Return 400
          Just url -> return . mkContent $ url

app :: Application
app = serve api server
