{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module ZoomHub.API where

import Control.Applicative
import Data.Aeson as Aeson
import Data.Char
import GHC.Generics
import Network.Wai
import Servant as S
import System.Directory
import ZoomHub.Types.Content

import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Either as Either
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Proxy as Proxy


-- Servant default handler type
type Handler a = Either.EitherT S.ServantErr IO a

-- API
type API =
      "v1" :> "content" :> S.Capture "id" String :> S.Get '[S.JSON] Content
 :<|> "v2" :> "content" :> S.QueryParam "url" String :> S.Get '[S.JSON] Content

-- Handlers
getContentFromFile :: String -> IO (Maybe Content)
getContentFromFile id = do
  cd <- getCurrentDirectory
  Aeson.decode <$> LBS.readFile (cd ++ "/data/content-by-id/" ++ id ++ ".json")

contentById :: String -> Handler Content
contentById id = do
  maybeContent <- IO.liftIO $ getContentFromFile id
  case maybeContent of
    Nothing -> Either.left S.err404{errBody="ID not found"}
    Just c  -> return c

contentByURL :: Maybe String -> Handler Content
contentByURL url = case url of
  Nothing  -> Either.left S.err400{
    errBody="Please provide an ID or `url` query parameter."
  }
  Just url -> return . mkContent $ url

-- API
api :: Proxy.Proxy API
api = Proxy.Proxy

server :: Server API
server = contentById
    :<|> contentByURL

app :: Application
app = serve api server
