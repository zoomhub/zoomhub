{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module ZoomHub.API
  ( app
  ) where

import           Control.Monad.IO.Class           (liftIO)
import           Control.Monad.Trans.Either       (EitherT, left)
import qualified Data.ByteString.Char8            as BC
import qualified Data.ByteString.Lazy             as BL
import qualified Data.ByteString.Lazy.Char8       as BLC
import           Data.Maybe                       (fromMaybe)
import           Data.Monoid                      ((<>))
import           Data.Proxy                       (Proxy (Proxy))
import           Network.Wai                      (Application)
import           Servant                          ((:<|>) (..), (:>), Capture,
                                                   Get, JSON, QueryParam, Raw,
                                                   ServantErr, Server, err301,
                                                   errHeaders, serve,
                                                   serveDirectory)
import           Servant.HTML.Lucid               (HTML)
import           System.Random                    (randomRIO)

import           ZoomHub.API.ContentTypes         (JavaScript)
import           ZoomHub.API.Errors               (error400, error404)
import           ZoomHub.Config                   (Config)
import qualified ZoomHub.Config                   as Config
import           ZoomHub.Storage.File             (create, getById, getByURL)
import           ZoomHub.Types.BaseURI            (BaseURI)
import           ZoomHub.Types.Content            (Content, fromInternal)
import           ZoomHub.Types.ContentBaseURI     (ContentBaseURI)
import           ZoomHub.Types.Embed              (Embed, mkEmbed)
import           ZoomHub.Types.EmbedDimension     (EmbedDimension)
import           ZoomHub.Types.EmbedId            (EmbedId, unEmbedId)
import qualified ZoomHub.Types.Internal.Content   as Internal
import           ZoomHub.Types.Internal.ContentId (ContentId, unId)
import           ZoomHub.Types.ViewContent        (ViewContent, mkViewContent)


-- Servant default handler type
type Handler a = EitherT ServantErr IO a

-- API
type API =
  -- TODO: Figure out how to route to `/`. Apparently `""` nor `"/"` works
  -- despite a hint here: https://git.io/vzEZx
       "health" :> Get '[HTML] String
  :<|> "version" :> Get '[HTML] String
  :<|> "v1" :> "content" :> Capture "id" ContentId :> Get '[JSON] Content
  :<|> "v1" :> "content" :> QueryParam "url" String :> Get '[JSON] Content
  :<|> Capture "id" EmbedId
       :> QueryParam "id" String
       :> QueryParam "width" EmbedDimension
       :> QueryParam "height" EmbedDimension
       :> Get '[JavaScript] Embed
  :<|> Capture "viewId" ContentId :> Get '[HTML] ViewContent
  :<|> Raw

-- API
api :: Proxy API
api = Proxy

server :: Config -> Server API
server config = health
           :<|> version (Config.version config)
           :<|> contentById baseURI contentBaseURI dataPath
           :<|> contentByURL config
           :<|> embed baseURI contentBaseURI dataPath viewerScript
           :<|> viewContentById baseURI contentBaseURI dataPath
           :<|> serveDirectory (Config.publicPath config)
  where
    baseURI = Config.baseURI config
    contentBaseURI = Config.contentBaseURI config
    dataPath = Config.dataPath config
    viewerScript = Config.openseadragonScript config

app :: Config -> Application
app config = serve api (server config)

-- Handlers
health :: Handler String
health = return "up"

version :: String -> Handler String
version = return

contentById :: BaseURI ->
               ContentBaseURI ->
               FilePath ->
               ContentId ->
               Handler Content
contentById baseURI contentBaseURI dataPath contentId = do
  maybeContent <- liftIO $ getById dataPath contentId
  case maybeContent of
    Nothing      -> left . error404 $ error404Message contentId
    Just content -> return $ fromInternal baseURI contentBaseURI content

contentByURL :: Config -> Maybe String -> Handler Content
contentByURL config maybeURL = case maybeURL of
  Nothing  -> left . error400 $ unwords
    [ "Missing ID or URL."
    , "Please provide ID, e.g. `/v1/content/<id>`,"
    , "or URL via `?url=<url>` query parameter."
    ]
  Just url -> do
      maybeContent <- liftIO $ getByURL (Config.dataPath config) url
      content <- case maybeContent of
        Nothing -> liftIO $ create config url
        Just c  -> return c
      redirect $ Internal.contentId content
      where
        -- NOTE: Enable Chrome developer console ‘[x] Disable cache’ to test
        -- permanent HTTP 301 redirects:
        redirect contentId =
          let location = BC.pack $ "/v1/content/" ++ unId contentId in
          left $ err301{
            -- HACK: Redirect using error: http://git.io/vBCz9
            errHeaders = [("Location", location)]
          }

embed :: BaseURI ->
         ContentBaseURI ->
         FilePath ->
         String ->
         EmbedId ->
         Maybe String ->
         Maybe EmbedDimension ->
         Maybe EmbedDimension ->
         Handler Embed
embed baseURI cBaseURI dataPath script embedId maybeId width height = do
  maybeContent <- liftIO $ getById dataPath contentId
  case maybeContent of
    Nothing      -> left . error404 $ error404Message contentId
    Just content -> do
      let randomIdRange = (100000, 999999) :: (Int, Int)
      randomId <- liftIO $ randomRIO randomIdRange
      let containerId = fromMaybe (defaultContainerId randomId) maybeId
          pContent = fromInternal baseURI cBaseURI content
      return $ mkEmbed baseURI cBaseURI containerId pContent script width height
  where
    contentId = unEmbedId embedId
    defaultContainerId n = "zoomhub-embed-" ++ show n


viewContentById :: BaseURI ->
                   ContentBaseURI ->
                   FilePath ->
                   ContentId ->
                   Handler ViewContent
viewContentById baseURI contentBaseURI dataPath contentId = do
  maybeContent <- liftIO $ getById dataPath contentId
  case maybeContent of
    Nothing -> left . error404 $ error404Message contentId
    Just c  -> do
      let content = fromInternal baseURI contentBaseURI c
      return $ mkViewContent baseURI content

-- Helpers
error404Message :: ContentId -> String
error404Message contentId = "No content with ID: " ++ unId contentId
