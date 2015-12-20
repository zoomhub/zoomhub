{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}

module ZoomHub.API where


import Data.Aeson as Aeson
import Network.Wai
import Servant as S

import qualified Control.Exception as E
import qualified Control.Monad as M
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Either as Either
import qualified "cryptonite" Crypto.Hash as Crypto
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Either as ET
import qualified Data.Proxy as Proxy
import qualified System.Directory as SD
import qualified System.IO.Error as SE
import qualified Text.Read as TR
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

-- Helpers
initalId :: Int
initalId = 0

createContentId :: IO ZH.ContentId
createContentId = do
  cd <- SD.getCurrentDirectory
  lastIdStr <- E.tryJust (M.guard . SE.isDoesNotExistError) (readFile (path cd))
  lastId <- case lastIdStr of
    ET.Left  _          -> initializeId cd
    ET.Right lastIdStr' ->
      case TR.readMaybe lastIdStr' of
        Nothing        -> initializeId cd
        Just numericId -> return numericId
  let context = H.hashidsSimple "zoomhub hash salt"
  let newId = C.unpack $ H.encode context lastId
  writeFile (path cd) (show $ lastId + 1)
  return $ ZH.ContentId newId
  where
    path :: String -> String
    path cd = cd ++ "/data/lastId.txt"
    initializeId cd = do
      writeFile (path cd) (show initalId)
      return initalId

mkContentFromURL :: String -> IO ZH.Content
mkContentFromURL url = do
  contentId <- createContentId
  return $ ZH.mkContent contentId url

getContentFromFile :: ZH.ContentId -> IO (Maybe ZH.Content)
getContentFromFile contentId = do
  cd <- SD.getCurrentDirectory
  f <- E.tryJust (M.guard . SE.isDoesNotExistError) (LBS.readFile (path cd))
  case f of
    ET.Left _  -> return Nothing
    ET.Right s -> return $ Aeson.decode s
  where path cd = cd ++ "/data/content-by-id/" ++ show contentId ++ ".json"

getContentIdFromURL :: CF.Credentials -> String -> IO (Maybe ZH.ContentId)
getContentIdFromURL creds url = do
  maybeContent <- CF.getContent creds urlPath
  case maybeContent of
    Nothing        -> return Nothing
    Just contentId -> return $ Just $ ZH.ContentId $ CL.unpack contentId
  where
    sha256 x = show (Crypto.hash $ C.pack x :: Crypto.Digest Crypto.SHA256)
    urlPath = "/content/content-by-url/" ++ (sha256 url) ++ ".txt"

-- Handlers
contentById :: ZH.ContentId -> Handler ZH.Content
contentById contentId = do
  maybeContent <- IO.liftIO $ getContentFromFile contentId
  case maybeContent of
    Nothing      -> Either.left S.err404{errBody = error404message}
    Just content -> return content
  where error404message = CL.pack $ "ID " ++ show contentId ++ " not found."

-- TODO: Use redirect to `contentById` instead:
contentByURL :: CF.Credentials -> Maybe String -> Handler ZH.Content
contentByURL creds maybeURL = case maybeURL of
  Nothing  -> Either.left S.err400{errBody = error400message}
  Just url -> do
    maybeContentId <- IO.liftIO $ getContentIdFromURL creds url
    case maybeContentId of
      -- TODO: Implement content conversion:
      -- Nothing -> do
      --   newContent <- IO.liftIO $ mkContentFromURL url
      --   redirect $ ZH.contentId newContent
      Nothing        -> Either.left $ S.err503{
        errBody="We cannot process your URL at this time."
      }
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
  where error400message = "Please provide an ID or `url` query parameter."

-- API
api :: Proxy.Proxy API
api = Proxy.Proxy

server :: CF.Credentials -> Server API
server creds = contentById
          :<|> contentByURL creds

app :: CF.Credentials -> Application
app creds = serve api (server creds)
