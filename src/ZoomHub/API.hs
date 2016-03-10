{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module ZoomHub.API
  ( app
  ) where

import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Trans.Either           (EitherT, left)
import qualified Data.ByteString.Char8                as BC
import           Data.Maybe                           (fromJust, fromMaybe)
import           Data.Proxy                           (Proxy (Proxy))
import           Network.URI                          (URI,
                                                       parseRelativeReference,
                                                       relativeTo)
import           Network.Wai                          (Application)
import           Network.Wai.Middleware.Cors          (simpleCors)
import           Servant                              ((:<|>) (..), (:>),
                                                       Capture, Get, JSON,
                                                       QueryParam, Raw,
                                                       ServantErr, Server,
                                                       err301, errHeaders,
                                                       serve)
import           Servant.HTML.Lucid                   (HTML)
import           System.Random                        (randomRIO)

import           ZoomHub.API.ContentTypes.JavaScript  (JavaScript)
import qualified ZoomHub.API.Errors                   as API
import           ZoomHub.API.Types.Callback           (Callback)
import           ZoomHub.API.Types.JSONP              (JSONP, mkJSONP)
import           ZoomHub.API.Types.NonRESTfulResponse (NonRESTfulResponse,
                                                       mkNonRESTful200)
import           ZoomHub.Config                       (Config)
import qualified ZoomHub.Config                       as Config
import           ZoomHub.Servant.RawCapture           (RawCapture)
import           ZoomHub.Servant.RequiredQueryParam   (RequiredQueryParam)
import           ZoomHub.Storage.File                 (getById, getByURL)
import           ZoomHub.Types.BaseURI                (BaseURI, unBaseURI)
import           ZoomHub.Types.Content                (Content, fromInternal)
import           ZoomHub.Types.ContentBaseURI         (ContentBaseURI)
import           ZoomHub.Types.Embed                  (Embed, mkEmbed)
import           ZoomHub.Types.EmbedDimension         (EmbedDimension)
import           ZoomHub.Types.EmbedId                (EmbedId, unEmbedId)
import qualified ZoomHub.Types.Internal.Content       as Internal
import           ZoomHub.Types.Internal.ContentId     (ContentId, unId)
import           ZoomHub.Types.Internal.ContentURI    (ContentURI)
import           ZoomHub.Types.ViewContent            (ViewContent,
                                                       mkViewContent)
import qualified ZoomHub.Web.Errors                   as Web
import           ZoomHub.Web.Static                   (serveDirectory)


-- Servant default handler type
type Handler a = EitherT ServantErr IO a

-- API
type API =
  -- TODO: Figure out how to route to `/`. Apparently `""` nor `"/"` works
  -- despite a hint here: https://git.io/vzEZx
  -- TODO: Use `ContentURI` instead of `String`:
       "health" :> Get '[HTML] String
  :<|> "version" :> Get '[HTML] String
  :<|> "v1" :> "content" :> Capture "id" ContentId :>
       RequiredQueryParam "callback" Callback :>
       Get '[JavaScript] (JSONP (NonRESTfulResponse Content))
  :<|> "v1" :> "content" :> Capture "id" ContentId :> Get '[JSON] Content
  :<|> "v1" :> "content" :> Capture "id" String :> Get '[JSON] Content
  :<|> "v1" :> "content" :> QueryParam "url" String :> Get '[JSON] Content
  :<|> Capture "id" EmbedId
       :> QueryParam "id" String
       :> QueryParam "width" EmbedDimension
       :> QueryParam "height" EmbedDimension
       :> Get '[JavaScript] Embed
  :<|> Capture "viewId" ContentId :> Get '[HTML] ViewContent
  :<|> RequiredQueryParam "url" ContentURI :> Get '[HTML] ViewContent
  -- Error handler for invalid URLs which will always match `String`:
  :<|> RequiredQueryParam "url" String :> Get '[HTML] ViewContent
  :<|> RawCapture "viewURI" ContentURI :> Get '[HTML] ViewContent
  :<|> Raw

-- API
api :: Proxy API
api = Proxy

server :: Config -> Server API
server config = health
           :<|> version (Config.version config)
           :<|> contentByIdJSONP baseURI contentBaseURI dataPath
           :<|> contentById baseURI contentBaseURI dataPath
           :<|> invalidContentId
           :<|> contentByURL baseURI dataPath
           :<|> embed baseURI contentBaseURI dataPath viewerScript
           :<|> viewContentById baseURI contentBaseURI dataPath
           :<|> viewContentByURL baseURI dataPath
           :<|> invalidURLParam
           :<|> viewContentByURL baseURI dataPath
           :<|> serveDirectory (Config.error404 config) publicPath
  where
    baseURI = Config.baseURI config
    contentBaseURI = Config.contentBaseURI config
    dataPath = Config.dataPath config
    publicPath = Config.publicPath config
    viewerScript = Config.openseadragonScript config

app :: Config -> Application
app config = simpleCors . logger $ serve api (server config)
  where logger = Config.logger config

-- Handlers
health :: Handler String
health = return "up"

version :: String -> Handler String
version = return

contentByIdJSONP :: BaseURI ->
                    ContentBaseURI ->
                    FilePath ->
                    ContentId ->
                    Callback ->
                    Handler (JSONP (NonRESTfulResponse Content))
contentByIdJSONP baseURI contentBaseURI dataPath contentId callback = do
  maybeContent <- liftIO $ getById dataPath contentId
  case maybeContent of
    -- TODO: Return non-RESTful error:
    Nothing      -> left . API.error404 $ error404Message contentId
    Just content -> do
      let publicContent = fromInternal baseURI contentBaseURI content
      return $ mkJSONP callback $ mkNonRESTful200 "content" publicContent

contentById :: BaseURI ->
               ContentBaseURI ->
               FilePath ->
               ContentId ->
               Handler Content
contentById baseURI contentBaseURI dataPath contentId = do
  maybeContent <- liftIO $ getById dataPath contentId
  case maybeContent of
    Nothing      -> left . API.error404 $ error404Message contentId
    Just content -> return $ fromInternal baseURI contentBaseURI content

invalidContentId :: String -> Handler Content
invalidContentId contentId = left . API.error404 $
  noContentWithIdMessage ++ contentId

contentByURL :: BaseURI -> FilePath -> Maybe String -> Handler Content
contentByURL baseURI dataPath maybeURL = case maybeURL of
  Nothing  -> left . API.error400 $ apiMissingIdOrURLMessage
  Just url -> do
    maybeContent <- liftIO $ getByURL dataPath url
    case maybeContent of
      Nothing      -> noNewContentErrorAPI
      Just content -> redirectToAPI baseURI (Internal.contentId content)

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
    Nothing      -> left . Web.error404 $ error404Message contentId
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
    Nothing -> left . Web.error404 $ error404Message contentId
    Just c  -> do
      let content = fromInternal baseURI contentBaseURI c
      return $ mkViewContent baseURI content

invalidURLParam :: String -> Handler ViewContent
invalidURLParam _ = left . Web.error400 $
  "Please give us the full URL, including ‘http://’ or ‘https://’."

-- TODO: Add support for submission, i.e. create content in the background:
viewContentByURL :: BaseURI -> FilePath -> ContentURI -> Handler ViewContent
viewContentByURL baseURI dataPath contentURI = do
  maybeContent <- liftIO $ getByURL dataPath (show contentURI)
  case maybeContent of
    Nothing -> noNewContentErrorWeb
    Just c  -> redirectToView baseURI (Internal.contentId c)

-- Helpers
error404Message :: ContentId -> String
error404Message contentId = noContentWithIdMessage ++ unId contentId

noContentWithIdMessage :: String
noContentWithIdMessage = "No content with ID: "

noNewContentErrorWeb :: Handler ViewContent
noNewContentErrorWeb = noNewContentError Web.error503

noNewContentErrorAPI :: Handler a
noNewContentErrorAPI = noNewContentError API.error503

noNewContentError :: (String -> ServantErr) -> Handler a
noNewContentError err =
  left . err $ "We are currently not processing new content."

apiMissingIdOrURLMessage :: String
apiMissingIdOrURLMessage = unwords
  [ "Missing ID or URL."
  , "Please provide ID, e.g. `/v1/content/<id>`,"
  , "or URL via `/v1/content?url=<url>` query parameter."
  ]

redirectToView :: BaseURI -> ContentId -> Handler ViewContent
redirectToView baseURI contentId =
  -- TODO: Look into Servant ‘Links’ for type safe link generation:
  redirect $ webRedirectURI baseURI contentId

redirectToAPI :: BaseURI -> ContentId -> Handler Content
redirectToAPI baseURI contentId =
  -- TODO: Look into Servant ‘Links’ for type safe link generation:
  redirect $ apiRedirectURI baseURI contentId

apiRedirectURI :: BaseURI -> ContentId -> URI
apiRedirectURI = redirectURI "/v1/content/"

webRedirectURI :: BaseURI -> ContentId -> URI
webRedirectURI = redirectURI "/"

redirectURI :: String -> BaseURI -> ContentId -> URI
redirectURI pathPrefix baseURI contentId =
  (fromJust . parseRelativeReference $ pathPrefix ++ unId contentId)
    `relativeTo` unBaseURI baseURI

-- NOTE: Enable Chrome developer console ‘[x] Disable cache’ to test
-- permanent HTTP 301 redirects:
redirect :: URI -> Handler a
redirect location =
  left $ err301{
    -- HACK: Redirect using error: http://git.io/vBCz9
    errHeaders = [("Location", BC.pack (show location))]
  }
