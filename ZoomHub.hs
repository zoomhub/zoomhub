{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
module ZoomHub where

import Control.Monad.Trans.Either
import Data.Char
import GHC.Generics
import Network.Wai
import Servant
import Types.Content



-- Servant default handler type
type Handler a = EitherT ServantErr IO a

-- API
type API = "v1" :> "content" :> Capture "id" String :> Get '[JSON] Content
      :<|> "v1" :> "content" :> QueryParam "url" String :> Get '[JSON] Content

api :: Proxy API
api = Proxy

server :: Server API
server = contentById
    :<|> contentByURL

  where contentById :: String -> Handler Content
        contentById id = return $ mkContent id
        contentByURL :: Maybe String -> Handler Content
        contentByURL url = case url of
          Nothing  -> return . mkContent $ "404" -- Return 400
          Just url -> return . mkContent $ url

app :: Application
app = serve api server
