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

import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Either as Either
import qualified Crypto.Hash as Crypto
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Proxy as Proxy
import qualified Web.Hashids as H
import qualified ZoomHub.Rackspace.CloudFiles as CF
import qualified ZoomHub.Types.Content as ZH


-- Servant default handler type
type Handler a = Either.EitherT S.ServantErr IO a

-- API
type API =
  "v1" :> "content" :> S.Capture "id" ZH.ContentId :> S.Get '[S.JSON] ZH.Content
  :<|>
  "v1" :> "content" :> S.QueryParam "url" String :> S.Get '[S.JSON] ZH.Content

-- Handlers
mkContentFromURL :: String -> ZH.Content
mkContentFromURL = ZH.mkContent (ZH.ContentId newId)
  where newId = let context = H.hashidsSimple "zoomhub hash salt" in
                C.unpack $ H.encode context 42

getContentFromFile :: ZH.ContentId -> IO (Maybe ZH.Content)
getContentFromFile contentId = do
  cd <- getCurrentDirectory
  Aeson.decode <$> LBS.readFile (
    cd ++ "/data/content-by-id/" ++ show contentId ++ ".json")

getContentIdFromURL :: CF.Credentials -> String -> IO (Maybe ZH.ContentId)
getContentIdFromURL creds url = do
  let urlHash = sha256 url
  let urlPath = "/content/content-by-url/" ++ urlHash ++ ".txt"
  maybeContent <- CF.getContent creds urlPath
  case maybeContent of
    Nothing        -> return Nothing
    Just contentId -> return $ Just $ ZH.ContentId $ CL.unpack contentId
  where
    sha256 :: String -> String
    sha256 x = show (Crypto.hash $ C.pack x :: Crypto.Digest Crypto.SHA256)

contentById :: ZH.ContentId -> Handler ZH.Content
contentById contentId = do
  maybeContent <- IO.liftIO $ getContentFromFile contentId
  case maybeContent of
    Nothing      -> Either.left S.err404{errBody = error404message}
    Just content -> return content
  where error404message = CL.pack $ "ID " ++ show contentId ++ " not found."

-- TODO: Use redirect to `contentById` instead:
contentByURL :: CF.Credentials -> Maybe String -> Handler ZH.Content
contentByURL creds url = case url of
  Nothing  -> Either.left S.err400{
    errBody="Please provide an ID or `url` query parameter."
  }
  Just url -> do
    maybeContentId <- IO.liftIO $ getContentIdFromURL creds url
    case maybeContentId of
      -- TODO: Implement content conversion:
      Nothing        -> return $ mkContentFromURL url
      Just contentId -> redirect contentId
      where
        -- NOTE: Enable Chrome developer console ‘[x] Disable cache’ to test
        -- permanent HTTP 301 redirects:
        redirect contentId =
          let location = C.pack $ "/v1/content/" ++ show contentId in
          Either.left $ S.err301{
            -- HACK: Redirect using error: http://git.io/vBCz9
            errHeaders = [("Location", location)]
          }

-- API
api :: Proxy.Proxy API
api = Proxy.Proxy

server :: CF.Credentials -> Server API
server creds = contentById
          :<|> contentByURL creds

app :: CF.Credentials -> Application
app creds = serve api (server creds)
