{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module ZoomHub.API where


import qualified Control.Exception                as E
import qualified Control.Monad                    as M
import qualified Control.Monad.IO.Class           as IO
import           Control.Monad.Trans.Either       (EitherT, left, right)
import qualified Data.ByteString.Char8            as BSC
import qualified Data.ByteString.Lazy             as LBS
import qualified Data.ByteString.Lazy.Char8       as CL
import           Data.Maybe                       (fromJust)
import qualified Data.Proxy                       as Proxy
import qualified Network.Wai                      as WAI
import           Servant                          ((:<|>) (..), (:>), Capture,
                                                   Get, JSON, QueryParam,
                                                   ServantErr, Server, err301,
                                                   err400, err404, errBody,
                                                   errHeaders, serve)
import qualified Servant                          as S
import qualified System.IO.Error                  as SE

import qualified ZoomHub.Config                   as C
import qualified ZoomHub.Rackspace.CloudFiles     as CF
import           ZoomHub.Storage.File             (create, getById, getByURL)
import qualified ZoomHub.Types.Content            as P
import qualified ZoomHub.Types.Internal.Content   as I
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
  maybeContent <- IO.liftIO $ getById dataPath contentId
  case maybeContent of
    Nothing      -> left err404{ errBody = error404message }
    Just content -> return $ P.fromInternal content
  where error404message = CL.pack $ "No content with ID: " ++ I.unId contentId

contentByURL :: C.Config -> CF.Metadata -> Maybe String -> Handler P.Content
contentByURL config meta maybeURL = case maybeURL of
  Nothing  -> left err400{ errBody = "Missing ID or URL." }
  Just url -> do
      maybeContent <- IO.liftIO $ getByURL config meta url
      content <- case maybeContent of
        Nothing -> IO.liftIO $ create config url
        Just c  -> return c
      redirect $ I.contentId content
      where
        -- NOTE: Enable Chrome developer console ‘[x] Disable cache’ to test
        -- permanent HTTP 301 redirects:
        redirect contentId =
          let location = BSC.pack $ "/v1/content/" ++ I.unId contentId in
          left $ err301{
            -- HACK: Redirect using error: http://git.io/vBCz9
            errHeaders = [("Location", location)]
          }

-- API
api :: Proxy.Proxy API
api = Proxy.Proxy

server :: C.Config -> CF.Metadata -> Server API
server config meta = contentById (C.dataPath config)
           :<|> contentByURL config meta

app :: C.Config -> CF.Metadata -> WAI.Application
app config meta = serve api (server config meta)
