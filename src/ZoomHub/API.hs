{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

module ZoomHub.API
  ( app,
  )
where

import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HS
import Data.Maybe (fromJust, fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Time as Time
import Network.Minio as Minio
import Network.Minio.S3API as S3
import Network.URI (URI, parseRelativeReference, relativeTo)
import Network.Wai (Application)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
  ( (:<|>) (..),
    (:>),
    Capture,
    Get,
    Handler,
    JSON,
    -- PlainText,
    -- Post,
    QueryParam,
    Raw,
    Server,
    ServerError,
    err301,
    errHeaders,
    serve,
  )
import Servant.HTML.Lucid (HTML)
import Squeal.PostgreSQL.Pool (Pool, runPoolPQ)
import System.Random (randomRIO)
import ZoomHub.API.ContentTypes.JavaScript (JavaScript)
import qualified ZoomHub.API.Errors as API
import qualified ZoomHub.API.JSONP.Errors as JSONP
import ZoomHub.API.Types.Callback (Callback)
import ZoomHub.API.Types.Content (Content, fromInternal)
import ZoomHub.API.Types.JSONP (JSONP, mkJSONP)
import ZoomHub.API.Types.NonRESTfulResponse
  ( NonRESTfulResponse,
    mkNonRESTful200,
    mkNonRESTful301,
    mkNonRESTful400,
    mkNonRESTful404,
    mkNonRESTful503,
  )
import ZoomHub.Config (Config, ProcessContent (..))
import qualified ZoomHub.Config as Config
import ZoomHub.Servant.RawCapture (RawCapture)
import ZoomHub.Servant.RequiredQueryParam (RequiredQueryParam)
import ZoomHub.Storage.PostgreSQL as PG
import ZoomHub.Storage.PostgreSQL (Connection)
import ZoomHub.Types.BaseURI (BaseURI, unBaseURI)
import qualified ZoomHub.Types.Content as Internal
import ZoomHub.Types.ContentBaseURI (ContentBaseURI)
import ZoomHub.Types.ContentId (ContentId, unContentId)
import ZoomHub.Types.ContentURI (ContentURI)
import ZoomHub.Types.StaticBaseURI (StaticBaseURI)
import ZoomHub.Utils (lenientDecodeUtf8)
import qualified ZoomHub.Web.Errors as Web
import ZoomHub.Web.Static (serveDirectory)
import ZoomHub.Web.Types.Embed (Embed, mkEmbed)
import ZoomHub.Web.Types.EmbedDimension (EmbedDimension)
import ZoomHub.Web.Types.EmbedId (EmbedId, unEmbedId)
import ZoomHub.Web.Types.ViewContent (ViewContent, mkViewContent)

-- API
type API =
  -- TODO: Route to homepage (`/`) using: `:<|> Get '[HTML]`
  -- Meta
  "health" :> Get '[HTML] String
    :<|> "version" :> Get '[HTML] String
    -- JSONP: ID
    :<|> "v1" :> "content"
      :> Capture "id" ContentId
      :> RequiredQueryParam "callback" Callback
      :> Get '[JavaScript] (JSONP (NonRESTfulResponse Content))
    -- JSONP: Error: ID
    :<|> "v1" :> "content"
      :> Capture "id" String
      :> RequiredQueryParam "callback" Callback
      :> Get '[JavaScript] (JSONP (NonRESTfulResponse String))
    -- JSONP: URL
    :<|> "v1" :> "content"
      :> RequiredQueryParam "url" ContentURI
      :> RequiredQueryParam "callback" Callback
      :> Get '[JavaScript] (JSONP (NonRESTfulResponse Content))
    -- JSONP: Error: URL
    :<|> "v1" :> "content"
      :> QueryParam "url" String
      :> RequiredQueryParam "callback" Callback
      :> Get '[JavaScript] (JSONP (NonRESTfulResponse String))
    -- API: RESTful: Upload
    :<|> "v1" :> "content" :> "upload" :> Get '[JSON] (HashMap Text Text)
    -- API: RESTful: ID
    :<|> "v1" :> "content" :> Capture "id" ContentId :> Get '[JSON] Content
    -- API: RESTful: Error: ID
    :<|> "v1" :> "content" :> Capture "id" String :> Get '[JSON] Content
    -- API: RESTful: URL
    :<|> "v1" :> "content"
      :> RequiredQueryParam "url" ContentURI
      :> Get '[JSON] Content
    -- API: RESTful: Error: URL
    :<|> "v1" :> "content"
      :> QueryParam "url" String
      :> Get '[JSON] Content
    -- Embed
    :<|> Capture "embedId" EmbedId
      :> QueryParam "id" String
      :> QueryParam "width" EmbedDimension
      :> QueryParam "height" EmbedDimension
      :> Get '[JavaScript] Embed
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
    :<|> jsonpContentById baseURI contentBaseURI dbConnPool
    :<|> jsonpInvalidContentId
    :<|> jsonpContentByURL baseURI contentBaseURI dbConnPool
    :<|> jsonpInvalidRequest
    -- API: RESTful
    :<|> restUpload
    :<|> restContentById baseURI contentBaseURI dbConnPool
    :<|> restInvalidContentId
    :<|> restContentByURL baseURI dbConnPool processContent
    :<|> restInvalidRequest
    -- Web: Embed
    :<|> webEmbed baseURI contentBaseURI staticBaseURI dbConnPool viewerScript
    -- Web: View
    :<|> webContentById baseURI contentBaseURI dbConnPool
    :<|> webContentByURL baseURI dbConnPool
    :<|> webInvalidURLParam
    :<|> webContentByURL baseURI dbConnPool
    -- Web: Static files
    :<|> serveDirectory (Config.error404 config) publicPath
  where
    baseURI = Config.baseURI config
    contentBaseURI = Config.contentBaseURI config
    dbConnPool = Config.dbConnPool config
    processContent = Config.processContent config
    publicPath = Config.publicPath config
    staticBaseURI = Config.staticBaseURI config
    viewerScript = Config.openSeadragonScript config

-- App
app :: Config -> Application
app config = logger . simpleCors $ serve api (server config)
  where
    logger = Config.logger config

-- Handlers

-- Meta
health :: Handler String
health = return "up"

version :: String -> Handler String
version = return

-- API: JSONP
jsonpContentById ::
  BaseURI ->
  ContentBaseURI ->
  Pool Connection ->
  ContentId ->
  Callback ->
  Handler (JSONP (NonRESTfulResponse Content))
jsonpContentById baseURI contentBaseURI dbConnPool contentId callback = do
  maybeContent <- liftIO $ runPoolPQ (PG.getById' contentId) dbConnPool
  case maybeContent of
    Nothing ->
      throwError $ JSONP.mkError
        $ mkJSONP callback
        $ mkNonRESTful404
        $ contentNotFoundMessage contentId
    Just content -> do
      let publicContent = fromInternal baseURI contentBaseURI content
      return $ mkJSONP callback $ mkNonRESTful200 "content" publicContent

jsonpContentByURL ::
  BaseURI ->
  ContentBaseURI ->
  Pool Connection ->
  ContentURI ->
  Callback ->
  Handler (JSONP (NonRESTfulResponse Content))
jsonpContentByURL baseURI contentBaseURI dbConnPool url callback = do
  maybeContent <- liftIO $ runPoolPQ (PG.getByURL' url) dbConnPool
  case maybeContent of
    Nothing ->
      throwError . JSONP.mkError $
        mkJSONP callback (mkNonRESTful503 noNewContentErrorMessage)
    Just content -> do
      let publicContent = fromInternal baseURI contentBaseURI content
          redirectLocation = apiRedirectURI baseURI contentId
          contentId = Internal.contentId content
      return $ mkJSONP callback $
        mkNonRESTful301 "content" publicContent redirectLocation

jsonpInvalidContentId ::
  String ->
  Callback ->
  Handler (JSONP (NonRESTfulResponse String))
jsonpInvalidContentId contentId callback =
  return $ mkJSONP callback (mkNonRESTful404 message)
  where
    message = noContentWithIdMessage contentId

jsonpInvalidRequest ::
  Maybe String ->
  Callback ->
  Handler (JSONP (NonRESTfulResponse String))
jsonpInvalidRequest maybeURL callback =
  case maybeURL of
    Nothing ->
      return $ mkJSONP callback (mkNonRESTful400 apiMissingIdOrURLMessage)
    Just _ ->
      return $ mkJSONP callback (mkNonRESTful400 invalidURLErrorMessage)

restUpload :: Handler (HashMap Text Text)
restUpload = do
  currentTime <- liftIO Time.getCurrentTime
  let expiryTime = Time.addUTCTime Time.nominalDay currentTime
      bucket = "sources-development.zoomhub.net"
      key = "uploads/test-" <> (T.pack $ show currentTime)
      s3URL = "http://" <> bucket <> ".s3.us-east-2.amazonaws.com" <> "/" <> key
      ePolicy =
        S3.newPostPolicy
          expiryTime
          [ S3.ppCondBucket bucket,
            S3.ppCondKey key,
            S3.ppCondContentLengthRange minUploadSizeBytes maxUploadSizeBytes,
            S3.ppCondContentType "image/",
            PPCEquals
              "success_action_redirect"
              ("http://localhost:8000/v1/content?url=" <> s3URL)
          ]
  case ePolicy of
    Left policyError ->
      return $ HS.singleton "error" (T.pack $ show policyError)
    Right policy -> do
      mAwsCredentials <- liftIO Minio.fromAWSEnv
      case mAwsCredentials of
        Nothing ->
          return $ HS.singleton "error" "missing credentials"
        Just awsCredentials -> do
          -- TODO: `success_action_redirect`
          let connectInfo = setRegion "us-east-2" $ setCreds awsCredentials Minio.awsCI
          result <- liftIO $ runMinio connectInfo (S3.presignedPostPolicy policy)
          case result of
            Left minioErr ->
              return $ HS.singleton "error" (T.pack $ show minioErr)
            Right (url, formData) -> do
              return $ lenientDecodeUtf8 <$> (HS.insert "url" url $ formData)
  where
    minUploadSizeBytes = 1
    maxUploadSizeBytes = 512 * 1024 * 1024

restContentById ::
  BaseURI ->
  ContentBaseURI ->
  Pool Connection ->
  ContentId ->
  Handler Content
restContentById baseURI contentBaseURI dbConnPool contentId = do
  maybeContent <- liftIO $ runPoolPQ (PG.getById' contentId) dbConnPool
  case maybeContent of
    Nothing -> throwError . API.error404 $ contentNotFoundMessage contentId
    Just content -> return $ fromInternal baseURI contentBaseURI content

restInvalidContentId :: String -> Handler Content
restInvalidContentId contentId =
  throwError . API.error404 $ noContentWithIdMessage contentId

restContentByURL ::
  BaseURI ->
  Pool Connection ->
  ProcessContent ->
  ContentURI ->
  Handler Content
restContentByURL baseURI dbConnPool processContent url = do
  maybeContent <- liftIO $ runPoolPQ (PG.getByURL' url) dbConnPool
  case maybeContent of
    Nothing -> do
      mNewContent <- case processContent of
        ProcessExistingAndNewContent ->
          liftIO $ runPoolPQ (PG.initialize url) dbConnPool
        _ ->
          noNewContentErrorAPI
      case mNewContent of
        Just newContent ->
          redirectToAPI baseURI (Internal.contentId newContent)
        Nothing ->
          throwError . API.error503 $ failedToCreateContentErrorMessage
    Just content ->
      redirectToAPI baseURI (Internal.contentId content)

restInvalidRequest :: Maybe String -> Handler Content
restInvalidRequest maybeURL = case maybeURL of
  Nothing -> throwError . API.error400 $ apiMissingIdOrURLMessage
  Just _ -> throwError . API.error400 $ invalidURLErrorMessage

-- Web: Embed
webEmbed ::
  BaseURI ->
  ContentBaseURI ->
  StaticBaseURI ->
  Pool Connection ->
  String ->
  EmbedId ->
  Maybe String ->
  Maybe EmbedDimension ->
  Maybe EmbedDimension ->
  Handler Embed
webEmbed
  baseURI
  contentBaseURI
  staticBaseURI
  dbConnPool
  viewerScript
  embedId
  maybeId
  width
  height = do
    maybeContent <- liftIO $ runPoolPQ (PG.getById' contentId) dbConnPool
    case maybeContent of
      Nothing -> throwError . Web.error404 $ contentNotFoundMessage contentId
      Just content -> do
        let randomIdRange = (100000, 999999) :: (Int, Int)
        randomId <- liftIO $ randomRIO randomIdRange
        let containerId = fromMaybe (defaultContainerId randomId) maybeId
            pContent = fromInternal baseURI contentBaseURI content
        return $
          mkEmbed
            baseURI
            staticBaseURI
            containerId
            pContent
            viewerScript
            width
            height
    where
      contentId = unEmbedId embedId
      defaultContainerId n = "zoomhub-embed-" ++ show n

-- Web: View
webContentById ::
  BaseURI ->
  ContentBaseURI ->
  Pool Connection ->
  ContentId ->
  Handler ViewContent
webContentById baseURI contentBaseURI dbConnPool contentId = do
  maybeContent <- liftIO $ runPoolPQ (PG.getById contentId) dbConnPool
  case maybeContent of
    Nothing -> throwError . Web.error404 $ contentNotFoundMessage contentId
    Just c -> do
      let content = fromInternal baseURI contentBaseURI c
      return $ mkViewContent baseURI content

-- TODO: Add support for submission, i.e. create content in the background:
webContentByURL ::
  BaseURI ->
  Pool Connection ->
  ContentURI ->
  Handler ViewContent
webContentByURL baseURI dbConnPool contentURI = do
  maybeContent <- liftIO $ runPoolPQ (PG.getByURL contentURI) dbConnPool
  case maybeContent of
    Nothing -> noNewContentErrorWeb
    Just c -> redirectToView baseURI (Internal.contentId c)

webInvalidURLParam :: String -> Handler ViewContent
webInvalidURLParam _ = throwError . Web.error400 $ invalidURLErrorMessage

-- Helpers
contentNotFoundMessage :: ContentId -> String
contentNotFoundMessage contentId =
  noContentWithIdMessage (unContentId contentId)

noContentWithIdMessage :: String -> String
noContentWithIdMessage contentId = "No content with ID: " ++ contentId

noNewContentErrorWeb :: Handler ViewContent
noNewContentErrorWeb = noNewContentError Web.error503

noNewContentErrorAPI :: Handler a
noNewContentErrorAPI = noNewContentError API.error503

noNewContentError :: (String -> ServerError) -> Handler a
noNewContentError err =
  throwError . err $ noNewContentErrorMessage

noNewContentErrorMessage :: String
noNewContentErrorMessage = "We are currently not processing new content."

failedToCreateContentErrorMessage :: String
failedToCreateContentErrorMessage =
  "Sorry, we failed to process your submission at this time. Please try again later."

apiMissingIdOrURLMessage :: String
apiMissingIdOrURLMessage =
  unwords
    [ "Missing ID or URL.",
      "Please provide ID, e.g. `/v1/content/<id>`,",
      "or URL via `/v1/content?url=<url>` query parameter."
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
  (fromJust . parseRelativeReference $ pathPrefix ++ unContentId contentId)
    `relativeTo` unBaseURI baseURI

-- NOTE: Enable Chrome developer console ‘[x] Disable cache’ to test
-- permanent HTTP 301 redirects:
redirect :: URI -> Handler a
redirect location =
  throwError $
    err301
      { -- HACK: Redirect using error: http://git.io/vBCz9
        errHeaders = [("Location", BC.pack (show location))]
      }
