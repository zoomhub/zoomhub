{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ZoomHub.API where


import Control.Applicative
import Data.Aeson as Aeson
import Data.Char
import Network.Wai
import Servant as S
import System.Directory
import ZoomHub.Types.Content

import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Either as Either
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Proxy as Proxy
import qualified ZoomHub.Rackspace.CloudFiles as CF


-- Servant default handler type
type Handler a = Either.EitherT S.ServantErr IO a

-- API
type API =
      "v1" :> "content" :> S.Capture "id" String :> S.Get '[S.JSON] Content
 :<|> "v1" :> "content" :> S.QueryParam "url" String :> S.Get '[S.JSON] Content

-- Handlers
getContentFromFile :: String -> IO (Maybe Content)
getContentFromFile id = do
  cd <- getCurrentDirectory
  Aeson.decode <$> LBS.readFile (cd ++ "/data/content-by-id/" ++ id ++ ".json")

getContentFromURL :: String -> IO (Maybe Content)
getContentFromURL url = do
  let urlHash = sha256 url
  let urlPath = "/content/content-by-url/" ++ urlHash ++ ".txt"
  let creds = CF.Credentials "<TODO>" "<TODO>"
  maybeContentId <- CF.getContent creds urlPath
  case maybeContentId of
    Nothing        -> return Nothing
    Just contentId -> getContentFromFile $ CL.unpack contentId
  where
    sha256 :: String -> String
    sha256 x = show (Crypto.hash $ C.pack x :: Crypto.Digest Crypto.SHA256)

contentById :: String -> Handler Content
contentById id = do
  maybeContent <- IO.liftIO $ getContentFromFile id
  case maybeContent of
    Nothing      -> Either.left S.err404{errBody="ID not found"}
    Just content -> return content

contentByURL :: Maybe String -> Handler Content
contentByURL url = case url of
  Nothing  -> Either.left S.err400{
    errBody="Please provide an ID or `url` query parameter."
  }
  Just url -> do
    maybeContent <- IO.liftIO $ getContentFromURL url
    case maybeContent of
      Nothing      -> Either.left $ S.err404{errBody="URL not found"}
      Just content -> return content

-- API
api :: Proxy.Proxy API
api = Proxy.Proxy

server :: Server API
server = contentById
    :<|> contentByURL

app :: Application
app = serve api server
