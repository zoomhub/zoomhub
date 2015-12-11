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
import qualified ZoomHub.Rackspace.CloudFiles as CF
import qualified ZoomHub.Types.Content as ZH


-- Servant default handler type
type Handler a = Either.EitherT S.ServantErr IO a

-- API
type API =
  "v1" :> "content" :> S.Capture "id" String :> S.Get '[S.JSON] ZH.Content
  :<|>
  "v1" :> "content" :> S.QueryParam "url" String :> S.Get '[S.JSON] ZH.Content

-- Handlers
getContentFromFile :: String -> IO (Maybe ZH.Content)
getContentFromFile id = do
  cd <- getCurrentDirectory
  Aeson.decode <$> LBS.readFile (cd ++ "/data/content-by-id/" ++ id ++ ".json")

getContentFromURL :: CF.Credentials -> String -> IO (Maybe ZH.Content)
getContentFromURL creds url = do
  let urlHash = sha256 url
  let urlPath = "/content/content-by-url/" ++ urlHash ++ ".txt"
  maybeContentId <- CF.getContent creds urlPath
  case maybeContentId of
    Nothing        -> return Nothing
    Just contentId -> getContentFromFile $ CL.unpack contentId
  where
    sha256 :: String -> String
    sha256 x = show (Crypto.hash $ C.pack x :: Crypto.Digest Crypto.SHA256)

contentById :: String -> Handler ZH.Content
contentById id = do
  maybeContent <- IO.liftIO $ getContentFromFile id
  case maybeContent of
    Nothing      -> Either.left S.err404{
      errBody=CL.pack $ "ID " ++ id ++ " not found."
    }
    Just content -> return content

-- TODO: Use redirect to `contentById` instead:
contentByURL :: CF.Credentials -> Maybe String -> Handler ZH.Content
contentByURL creds url = case url of
  Nothing  -> Either.left S.err400{
    errBody="Please provide an ID or `url` query parameter."
  }
  Just url -> do
    maybeContent <- IO.liftIO $ getContentFromURL creds url
    case maybeContent of
      -- TODO: Implement content conversion:
      Nothing      -> Either.left $ S.err404{
        errBody="URL not found"
      }
      Just (ZH.ContentId contentId) ->
        -- TODO: Use 301 permanent redirect once testing is complete:
        Either.left $ S.err302{
          -- HACK: Redirect using error: http://git.io/vBCz9
          errHeaders = [("Location", C.pack $ "/v1/content/" ++ contentId)]
        }

-- API
api :: Proxy.Proxy API
api = Proxy.Proxy

server :: CF.Credentials -> Server API
server creds = contentById
          :<|> contentByURL creds

app :: CF.Credentials -> Application
app creds = serve api (server creds)
