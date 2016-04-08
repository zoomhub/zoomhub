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
import           Database.SQLite.Simple               (Connection)
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
import qualified ZoomHub.API.JSONP.Errors             as JSONP
import           ZoomHub.API.Types.Callback           (Callback)
import           ZoomHub.API.Types.Content            (Content, fromInternal)
import           ZoomHub.API.Types.JSONP              (JSONP, mkJSONP)
import           ZoomHub.API.Types.NonRESTfulResponse (NonRESTfulResponse,
                                                       mkNonRESTful200,
                                                       mkNonRESTful301,
                                                       mkNonRESTful400,
                                                       mkNonRESTful404,
                                                       mkNonRESTful503)
import           ZoomHub.Config                       (Config, NewContentStatus (NewContentAllowed))
import qualified ZoomHub.Config                       as Config
import           ZoomHub.Servant.RawCapture           (RawCapture)
import           ZoomHub.Servant.RequiredQueryParam   (RequiredQueryParam)
import           ZoomHub.Storage.SQLite               (create, getById,
                                                       getById', getByURL,
                                                       getByURL')
import           ZoomHub.Types.BaseURI                (BaseURI, unBaseURI)
import qualified ZoomHub.Types.Content                as Internal
import           ZoomHub.Types.ContentBaseURI         (ContentBaseURI)
import           ZoomHub.Types.ContentId              (ContentId, unId)
import           ZoomHub.Types.ContentURI             (ContentURI)
import           ZoomHub.Types.StaticBaseURI          (StaticBaseURI)
import qualified ZoomHub.Web.Errors                   as Web
import           ZoomHub.Web.Static                   (serveDirectory)
import           ZoomHub.Web.Types.Embed              (Embed, mkEmbed)
import           ZoomHub.Web.Types.EmbedDimension     (EmbedDimension)
import           ZoomHub.Web.Types.EmbedId            (EmbedId, unEmbedId)
import           ZoomHub.Web.Types.ViewContent        (ViewContent,
                                                       mkViewContent)


-- Servant default handler type
type Handler a = EitherT ServantErr IO a

-- API
type API =
  -- TODO: Figure out how to route to `/`. Apparently `""` nor `"/"` works
  -- despite a hint here: https://git.io/vzEZx. Solution: `:<|> Get '[HTML]`
  -- TODO: Use `ContentURI` instead of `String`:
  -- Meta
       "health" :> Get '[HTML] String
  :<|> "version" :> Get '[HTML] String
  -- JSONP: ID
  :<|> "v1" :> "content" :>
       Capture "id" ContentId :>
       RequiredQueryParam "callback" Callback :>
       Get '[JavaScript] (JSONP (NonRESTfulResponse Content))
  -- JSONP: Error: ID
  :<|> "v1" :> "content" :>
       Capture "id" String :>
       RequiredQueryParam "callback" Callback :>
       Get '[JavaScript] (JSONP (NonRESTfulResponse String))
  -- JSONP: URL
  :<|> "v1" :> "content" :>
       RequiredQueryParam "url" ContentURI :>
       RequiredQueryParam "callback" Callback :>
       Get '[JavaScript] (JSONP (NonRESTfulResponse Content))
  -- JSONP: Error: URL
  :<|> "v1" :> "content" :>
       QueryParam "url" String :>
       RequiredQueryParam "callback" Callback :>
       Get '[JavaScript] (JSONP (NonRESTfulResponse String))
  -- API: RESTful: ID
  :<|> "v1" :> "content" :> Capture "id" ContentId :> Get '[JSON] Content
  -- API: RESTful: Error: ID
  :<|> "v1" :> "content" :> Capture "id" String :> Get '[JSON] Content
  -- API: RESTful: URL
  :<|> "v1" :> "content" :>
       RequiredQueryParam "url" ContentURI :>
       Get '[JSON] Content
  -- API: RESTful: Error: URL
  :<|> "v1" :> "content" :>
       QueryParam "url" String :>
       Get '[JSON] Content
  -- Embed
  :<|> Capture "embedId" EmbedId :>
       QueryParam "id" String :>
       QueryParam "width" EmbedDimension :>
       QueryParam "height" EmbedDimension :>
       Get '[JavaScript] Embed
  -- Web: View
  :<|> Capture "viewId" ContentId :> Get '[HTML] ViewContent
  :<|> RequiredQueryParam "url" ContentURI :> Get '[HTML] ViewContent
  -- Web: View: Error: Invalid URL
  :<|> RequiredQueryParam "url" String :> Get '[HTML] ViewContent
  -- Web: Shortcut: `http://zoomhub.net/http://example.com`:
  :<|> RawCapture "viewURI" ContentURI :> Get '[HTML] ViewContent
  -- Static files
  :<|> Raw

-- API
api :: Proxy API
api = Proxy

server :: Config -> Server API
server config =
    -- Meta
         health
    :<|> version (Config.version config)
    -- API: JSONP
    :<|> jsonpContentById baseURI contentBaseURI dbConn
    :<|> jsonpInvalidContentId
    :<|> jsonpContentByURL baseURI contentBaseURI dbConn
    :<|> jsonpInvalidRequest
    -- API: RESTful
    :<|> restContentById baseURI contentBaseURI dbConn
    :<|> restInvalidContentId
    :<|> restContentByURL baseURI dbConn encodeId newContentStatus
    :<|> restInvalidRequest
    -- Web: Embed
    :<|> webEmbed baseURI contentBaseURI staticBaseURI dbConn viewerScript
    -- Web: View
    :<|> webContentById baseURI contentBaseURI dbConn
    :<|> webContentByURL baseURI dbConn
    :<|> webInvalidURLParam
    :<|> webContentByURL baseURI dbConn
    -- Web: Static files
    :<|> serveDirectory (Config.error404 config) publicPath
  where
    baseURI = Config.baseURI config
    contentBaseURI = Config.contentBaseURI config
    staticBaseURI = Config.staticBaseURI config
    dbConn = Config.dbConnection config
    encodeId = Config.encodeId config
    newContentStatus = Config.newContentStatus config
    publicPath = Config.publicPath config
    viewerScript = Config.openseadragonScript config

-- App
app :: Config -> Application
app config = logger . simpleCors $ serve api (server config)
  where logger = Config.logger config

-- Handlers

-- Meta
health :: Handler String
health = return "up"

version :: String -> Handler String
version = return

-- API: JSONP
jsonpContentById :: BaseURI ->
                    ContentBaseURI ->
                    Connection ->
                    ContentId ->
                    Callback ->
                    Handler (JSONP (NonRESTfulResponse Content))
jsonpContentById baseURI contentBaseURI dbConn contentId callback = do
  maybeContent <- liftIO $ getById' dbConn contentId
  case maybeContent of
    Nothing      -> left $ JSONP.mkError $
      mkJSONP callback $ mkNonRESTful404 $ contentNotFoundMessage contentId
    Just content -> do
      let publicContent = fromInternal baseURI contentBaseURI content
      return $ mkJSONP callback $ mkNonRESTful200 "content" publicContent

jsonpContentByURL :: BaseURI ->
                     ContentBaseURI ->
                     Connection ->
                     ContentURI ->
                     Callback ->
                     Handler (JSONP (NonRESTfulResponse Content))
jsonpContentByURL baseURI contentBaseURI dbConn url callback = do
  maybeContent <- liftIO $ getByURL' dbConn url
  case maybeContent of
    Nothing      -> left . JSONP.mkError $
      mkJSONP callback (mkNonRESTful503 noNewContentErrorMessage)
    Just content -> do
      let publicContent = fromInternal baseURI contentBaseURI content
          redirectLocation = apiRedirectURI baseURI contentId
          contentId = Internal.contentId content
      return $ mkJSONP callback $
        mkNonRESTful301 "content" publicContent redirectLocation

jsonpInvalidContentId :: String ->
                         Callback ->
                         Handler (JSONP (NonRESTfulResponse String))
jsonpInvalidContentId contentId callback =
    return $ mkJSONP callback (mkNonRESTful404 message)
  where message = noContentWithIdMessage contentId

jsonpInvalidRequest :: Maybe String ->
                       Callback ->
                       Handler (JSONP (NonRESTfulResponse String))
jsonpInvalidRequest maybeURL callback =
  case maybeURL of
    Nothing ->
      return $ mkJSONP callback (mkNonRESTful400 apiMissingIdOrURLMessage)
    Just _ ->
      return $ mkJSONP callback (mkNonRESTful400 invalidURLErrorMessage)

-- API: RESTful
restContentById :: BaseURI ->
                   ContentBaseURI ->
                   Connection ->
                   ContentId ->
                   Handler Content
restContentById baseURI contentBaseURI dbConn contentId = do
  maybeContent <- liftIO $ getById' dbConn contentId
  case maybeContent of
    Nothing      -> left . API.error404 $ contentNotFoundMessage contentId
    Just content -> return $ fromInternal baseURI contentBaseURI content

restInvalidContentId :: String -> Handler Content
restInvalidContentId contentId =
  left . API.error404 $ noContentWithIdMessage contentId

restContentByURL :: BaseURI ->
                    Connection ->
                    (Integer -> String) ->
                    NewContentStatus ->
                    ContentURI ->
                    Handler Content
restContentByURL baseURI dbConn encodeId newContentStatus url = do
  maybeContent <- liftIO $ getByURL' dbConn url
  case maybeContent of
    Nothing      -> do
      newContent <- case newContentStatus of
        NewContentAllowed -> liftIO $ create dbConn encodeId url
        _ -> noNewContentErrorAPI
      redirectToAPI baseURI (Internal.contentId newContent)
    Just content -> redirectToAPI baseURI (Internal.contentId content)

restInvalidRequest :: Maybe String -> Handler Content
restInvalidRequest maybeURL = case maybeURL of
  Nothing  -> left . API.error400 $ apiMissingIdOrURLMessage
  Just _   -> left . API.error400 $ invalidURLErrorMessage

-- Web: Embed
webEmbed :: BaseURI ->
            ContentBaseURI ->
            StaticBaseURI ->
            Connection ->
            String ->
            EmbedId ->
            Maybe String ->
            Maybe EmbedDimension ->
            Maybe EmbedDimension ->
            Handler Embed
webEmbed baseURI contentBaseURI staticBaseURI dbConn viewerScript embedId
         maybeId width height = do
  maybeContent <- liftIO $ getById' dbConn contentId
  case maybeContent of
    Nothing      -> left . Web.error404 $ contentNotFoundMessage contentId
    Just content -> do
      let randomIdRange = (100000, 999999) :: (Int, Int)
      randomId <- liftIO $ randomRIO randomIdRange
      let containerId = fromMaybe (defaultContainerId randomId) maybeId
          pContent = fromInternal baseURI contentBaseURI content
      return $ mkEmbed baseURI staticBaseURI containerId pContent
        viewerScript width height
  where
    contentId = unEmbedId embedId
    defaultContainerId n = "zoomhub-embed-" ++ show n

-- Web: View
webContentById :: BaseURI ->
                  ContentBaseURI ->
                  Connection ->
                  ContentId ->
                  Handler ViewContent
webContentById baseURI contentBaseURI dbConn contentId = do
  maybeContent <- liftIO $ getById dbConn contentId
  case maybeContent of
    Nothing -> left . Web.error404 $ contentNotFoundMessage contentId
    Just c  -> do
      let content = fromInternal baseURI contentBaseURI c
      return $ mkViewContent baseURI content

-- TODO: Add support for submission, i.e. create content in the background:
webContentByURL :: BaseURI -> Connection -> ContentURI -> Handler ViewContent
webContentByURL baseURI dbConn contentURI = do
  maybeContent <- liftIO $ getByURL dbConn contentURI
  case maybeContent of
    Nothing -> noNewContentErrorWeb
    Just c  -> redirectToView baseURI (Internal.contentId c)

webInvalidURLParam :: String -> Handler ViewContent
webInvalidURLParam _ = left . Web.error400 $ invalidURLErrorMessage

-- Helpers
contentNotFoundMessage :: ContentId -> String
contentNotFoundMessage contentId = noContentWithIdMessage (unId contentId)

noContentWithIdMessage :: String -> String
noContentWithIdMessage contentId = "No content with ID: " ++ contentId

noNewContentErrorWeb :: Handler ViewContent
noNewContentErrorWeb = noNewContentError Web.error503

noNewContentErrorAPI :: Handler a
noNewContentErrorAPI = noNewContentError API.error503

noNewContentError :: (String -> ServantErr) -> Handler a
noNewContentError err =
  left . err $ noNewContentErrorMessage

noNewContentErrorMessage :: String
noNewContentErrorMessage = "We are currently not processing new content."

apiMissingIdOrURLMessage :: String
apiMissingIdOrURLMessage = unwords
  [ "Missing ID or URL."
  , "Please provide ID, e.g. `/v1/content/<id>`,"
  , "or URL via `/v1/content?url=<url>` query parameter."
  ]

invalidURLErrorMessage :: String
invalidURLErrorMessage =
  "Please give us the full URL, including ‘http://’ or ‘https://’."

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
