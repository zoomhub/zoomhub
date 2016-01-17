{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ZoomHub.API where


import Servant((:<|>)(..),(:>), Capture, err301, err400, err404, errBody,
               errHeaders, Get, JSON, QueryParam, serve, ServantErr, Server)
import ZoomHub.Storage.File ( load
                            , getContentIdFromURL
                            , getContentPath
                            )
import Control.Monad.Trans.Either (EitherT, left, right)

import qualified Control.Concurrent.STM as STM
import qualified Control.Exception as E
import qualified Control.Monad as M
import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as Aeson
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Char8 as CL
import qualified Data.Proxy as Proxy
import qualified Network.Wai as WAI
import qualified Servant as S
import qualified System.IO.Error as SE
import qualified ZoomHub.Config as C
import qualified ZoomHub.Rackspace.CloudFiles as CF
import qualified ZoomHub.Types.Content as P
import qualified ZoomHub.Types.Internal.Content as I
import qualified ZoomHub.Types.Internal.ContentId as I


-- Servant default handler type
type Handler a = EitherT ServantErr IO a

-- API
type API =
  "v1" :> "content" :> Capture "id" I.ContentId :> Get '[JSON] P.Content
  :<|>
  "v1" :> "content" :> QueryParam "url" String :> Get '[JSON] P.Content

-- Handlers
contentById :: String -> I.ContentId -> Handler P.Content
contentById dataPath contentId = do
  maybeContent <- IO.liftIO $ load dataPath contentId
  case maybeContent of
    Nothing -> left err404{ errBody = error404message }
    Just c  -> return $ P.fromInternal c
  where error404message = CL.pack $ "No content with ID: " ++ I.unId contentId

contentByURL :: C.Config -> CF.Metadata -> Maybe String -> Handler P.Content
contentByURL config meta maybeURL = case maybeURL of
  Nothing -> left err400{ errBody = "Missing ID or URL." }
  Just url -> do
    maybeContentId <- IO.liftIO $ getContentIdFromURL meta url
    case maybeContentId of
      Nothing -> do
        newId <- IO.liftIO $ incrementAndGet $ C.lastId config
        let newContentId = I.fromInteger encodeIntegerId newId
        let newContent = I.fromURL newContentId url
        IO.liftIO $ LBS.writeFile (getContentPath dataPath newContentId)
          (Aeson.encodePretty' I.prettyEncodeConfig newContent)
        redirect $ newContentId
      Just contentId -> redirect contentId
      where
        -- NOTE: Enable Chrome developer console ‘[x] Disable cache’ to test
        -- permanent HTTP 301 redirects:
        redirect contentId =
          let location = BSC.pack $ "/v1/content/" ++ I.unId contentId in
          left $ err301{
            -- HACK: Redirect using error: http://git.io/vBCz9
            errHeaders = [("Location", location)]
          }
        incrementAndGet :: STM.TVar Integer -> IO Integer
        incrementAndGet tvar = STM.atomically $ do
          STM.modifyTVar tvar (+1)
          STM.readTVar tvar
        dataPath = C.dataPath config
        encodeIntegerId = C.encodeIntegerId config

-- API
api :: Proxy.Proxy API
api = Proxy.Proxy

server :: C.Config -> CF.Metadata -> Server API
server config meta = contentById (C.dataPath config)
           :<|> contentByURL config meta

app :: C.Config -> CF.Metadata -> WAI.Application
app config meta = serve api (server config meta)
