{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeOperators #-}

module ZoomHub.API where


import Servant((:<|>)(..),(:>))

import qualified "cryptonite" Crypto.Hash as Crypto
import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as E
import qualified Control.Monad as M
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Either as Either
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Either as ET
import qualified Data.Proxy as Proxy
import qualified Network.Wai as WAI
import qualified Servant as S
import qualified System.IO.Error as SE
import qualified Web.Hashids as H
import qualified ZoomHub.Config as C
import qualified ZoomHub.Rackspace.CloudFiles as CF
import qualified ZoomHub.Types.Content as P
import qualified ZoomHub.Types.Internal.Content as I


-- Servant default handler type
type Handler a = Either.EitherT S.ServantErr IO a

-- API
type API =
  "v1" :> "content" :> S.Capture "id" I.ContentId :> S.Get '[S.JSON] P.Content
  :<|>
  "v1" :> "content" :> S.QueryParam "url" String :> S.Get '[S.JSON] P.Content

-- Helpers
toHashId :: Integer -> String
toHashId intId =
  -- TODO: Move Hashid secret to config:
  let context = H.hashidsSimple "zoomhub hash salt" in
  C.unpack $ H.encode context (fromIntegral intId)

mkContentFromURL :: I.ContentId -> String -> I.Content
mkContentFromURL newId url = I.mkContent newId url

getContentPath :: String -> I.ContentId -> String
getContentPath dataPath contentId =
  dataPath ++ "/content-by-id/" ++ show contentId ++ ".json"

getContentFromFile :: String -> I.ContentId -> IO (Maybe I.Content)
getContentFromFile dataPath contentId = do
  f <- E.tryJust (M.guard . SE.isDoesNotExistError) (LBS.readFile contentPath)
  case f of
    ET.Left _  -> return Nothing
    ET.Right s -> return $ Aeson.decode s
  where contentPath = getContentPath dataPath contentId

getContentIdFromURL :: CF.Credentials -> String -> IO (Maybe I.ContentId)
getContentIdFromURL creds url = do
  maybeContent <- CF.getContent creds urlPath
  case maybeContent of
    Nothing        -> return Nothing
    Just contentId -> return $ Just $ I.ContentId $ CL.unpack contentId
  where
    sha256 x = show (Crypto.hash $ C.pack x :: Crypto.Digest Crypto.SHA256)
    urlPath = "/content/content-by-url/" ++ (sha256 url) ++ ".txt"

-- Handlers
contentById :: String -> I.ContentId -> Handler P.Content
contentById dataPath contentId = do
  maybeContent <- IO.liftIO $ getContentFromFile dataPath contentId
  case maybeContent of
    Nothing -> Either.left S.err404{S.errBody = error404message}
    Just c  -> return $ P.fromInternal c
  where error404message = CL.pack $ "ID " ++ show contentId ++ " not found."

contentByURL :: C.Config -> CF.Credentials -> Maybe String -> Handler P.Content
contentByURL config creds maybeURL = case maybeURL of
  Nothing -> Either.left S.err400{
    S.errBody = "Please provide an ID or `url` query parameter."
  }
  Just url -> do
    maybeContentId <- IO.liftIO $ getContentIdFromURL creds url
    case maybeContentId of
      Nothing -> do
        newId <- IO.liftIO $ incrementAndGet $ C.lastId config
        let newHashId = toHashId newId
        let newContent = mkContentFromURL (I.ContentId newHashId) url
        let newContentId = I.contentId newContent
        IO.liftIO $ LBS.writeFile (getContentPath dataPath newContentId)
          (Aeson.encodePretty' I.prettyEncodeConfig newContent)
        redirect $ newContentId
      Just contentId -> redirect contentId
      where
        -- NOTE: Enable Chrome developer console ‘[x] Disable cache’ to test
        -- permanent HTTP 301 redirects:
        redirect contentId =
          let location = C.pack $ "/v1/content/" ++ show contentId in
          Either.left $ S.err301{
            -- HACK: Redirect using error: http://git.io/vBCz9
            S.errHeaders = [("Location", location)]
          }
        incrementAndGet :: STM.TVar Integer -> IO Integer
        incrementAndGet tvar = STM.atomically $ do
          STM.modifyTVar tvar (+1)
          STM.readTVar tvar
        dataPath = C.dataPath config

-- API
api :: Proxy.Proxy API
api = Proxy.Proxy

server :: C.Config -> S.Server API
server config = contentById (C.dataPath config)
           :<|> contentByURL config creds
  where
    username = (C.raxUsername . C.rackspace) config
    apiKey = (C.raxApiKey . C.rackspace) config
    creds = CF.Credentials username apiKey

app :: C.Config -> WAI.Application
app config = S.serve api (server config)
