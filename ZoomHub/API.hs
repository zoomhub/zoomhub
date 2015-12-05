{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module ZoomHub.API where

import Control.Applicative
import Control.Monad.Trans.Either
import Data.Aeson as Aeson
import Data.Char
import GHC.Generics
import Network.Wai
import Servant
import System.Directory
import ZoomHub.Types.Content

import qualified Control.Monad.IO.Class as IO
import qualified Data.ByteString.Lazy as LBS


-- Servant default handler type
type Handler a = EitherT ServantErr IO a

-- API
type API = "v1" :> "content" :> Capture "id" String :> Get '[JSON] Content
      :<|> "v1" :> "content" :> QueryParam "url" String :> Get '[JSON] Content

-- Handlers
getContentFromFile :: String -> IO (Maybe Content)
getContentFromFile id = do
  cd <- getCurrentDirectory
  Aeson.decode <$> LBS.readFile (cd ++ "/data/content-by-id/" ++ id ++ ".json")

contentById :: String -> Handler Content
contentById id = do
  maybeContent <- IO.liftIO $ getContentFromFile id
  case maybeContent of
    Nothing -> left Servant.err404{errBody="ID not found"}
    Just c  -> return c

contentByURL :: Maybe String -> Handler Content
contentByURL url = case url of
  Nothing  -> left Servant.err400{
    errBody="Please provide an ID or `url` query parameter."
  }
  Just url -> return . mkContent $ url

-- API
api :: Proxy API
api = Proxy

server :: Server API
server = contentById
    :<|> contentByURL

app :: Application
app = serve api server
