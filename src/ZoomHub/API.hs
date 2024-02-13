{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ZoomHub.API
  ( app,
  )
where

import Control.Monad.Except (throwError, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=))
import qualified Data.ByteString.Char8 as BC
import Data.Foldable (fold, for_)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HS
import Data.Maybe (fromJust, fromMaybe)
import Data.Proxy (Proxy (Proxy))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Time as Time
import qualified Data.UUID.V4 as UUIDV4
import Network.Minio as Minio
  ( awsCI,
    fromAWSEnv,
    runMinio,
    setCreds,
    setRegion,
  )
import Network.Minio.S3API as S3
  ( PostPolicyCondition (PPCEquals),
    newPostPolicy,
    ppCondBucket,
    ppCondContentLengthRange,
    ppCondContentType,
    ppCondKey,
    presignedPostPolicy,
  )
import Network.URI (URI, parseRelativeReference, relativeTo)
import qualified Network.URI.Encode as URIEncode
import Network.Wai (Application)
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
  ( Capture,
    Context (EmptyContext, (:.)),
    Get,
    Handler,
    JSON,
    Put,
    QueryParam,
    Raw,
    ReqBody,
    Server,
    ServerError,
    err301,
    errHeaders,
    serveWithContext,
    (:<|>) (..),
    (:>),
  )
import Servant.Auth.Server
  ( Auth,
    AuthResult (Authenticated, BadPassword, Indefinite, NoSuchUser),
    BasicAuth,
    defaultCookieSettings,
    defaultJWTSettings,
    generateKey,
    wwwAuthenticatedErr,
  )
import Servant.HTML.Lucid (HTML)
import Squeal.PostgreSQL.Session.Pool (Pool, usingConnectionPool)
import System.Random (randomRIO)
import ZoomHub.API.ContentTypes.JavaScript (JavaScript)
import qualified ZoomHub.API.Errors as API
import qualified ZoomHub.API.JSONP.Errors as JSONP
import ZoomHub.API.Types.Callback (Callback)
import qualified ZoomHub.API.Types.Config as API
import ZoomHub.API.Types.Content (Content)
import qualified ZoomHub.API.Types.Content as Content
import ZoomHub.API.Types.ContentCompletion (ContentCompletion (..))
import qualified ZoomHub.API.Types.ContentCompletion as Completion
import qualified ZoomHub.API.Types.DeepZoomImageWithoutURL as DeepZoomImage
import ZoomHub.API.Types.JSONP (JSONP, mkJSONP)
import ZoomHub.API.Types.NonRESTfulResponse
  ( NonRESTfulResponse,
    mkNonRESTful200,
    mkNonRESTful301,
    mkNonRESTful400,
    mkNonRESTful404,
    mkNonRESTful503,
  )
import qualified ZoomHub.Authentication as Authentication
import ZoomHub.Config (Config)
import qualified ZoomHub.Config as Config
import qualified ZoomHub.Config.AWS as AWS
import ZoomHub.Config.ProcessContent (ProcessContent (..))
import ZoomHub.Config.Uploads (Uploads (..))
import qualified ZoomHub.Email as Email
import qualified ZoomHub.Email.Verification as Verification
import ZoomHub.Log.Logger (logWarning)
import ZoomHub.Servant.RawCapture (RawCapture)
import ZoomHub.Servant.RequiredQueryParam (RequiredQueryParam)
import ZoomHub.Storage.PostgreSQL as PG
import ZoomHub.Storage.PostgreSQL.GetRecent as PG
import ZoomHub.Types.BaseURI (BaseURI, unBaseURI)
import qualified ZoomHub.Types.Content as Internal
import ZoomHub.Types.ContentBaseURI (ContentBaseURI)
import ZoomHub.Types.ContentId (ContentId, unContentId)
import qualified ZoomHub.Types.ContentState as ContentState
import ZoomHub.Types.ContentURI (ContentURI)
import qualified ZoomHub.Types.Environment as Environment
import ZoomHub.Types.StaticBaseURI (StaticBaseURI)
import qualified ZoomHub.Types.VerificationError as VerificationError
import ZoomHub.Types.VerificationToken (VerificationToken)
import ZoomHub.Utils (lenientDecodeUtf8)
import qualified ZoomHub.Web.Errors as Web
import ZoomHub.Web.Page.EmbedContent (EmbedContent (..))
import qualified ZoomHub.Web.Page.EmbedContent as Page
import ZoomHub.Web.Page.ExploreRecentContent (ExploreRecentContent (..))
import qualified ZoomHub.Web.Page.ExploreRecentContent as Page
import qualified ZoomHub.Web.Page.VerifyContent as Page
import qualified ZoomHub.Web.Page.VerifyContent as VerificationResult
import ZoomHub.Web.Page.ViewContent (ViewContent (..))
import qualified ZoomHub.Web.Page.ViewContent as Page
import ZoomHub.Web.Static (serveDirectory)
import ZoomHub.Web.Types.Embed (Embed (..))
import ZoomHub.Web.Types.EmbedBackground (EmbedBackground)
import ZoomHub.Web.Types.EmbedBorder (EmbedBorder)
import ZoomHub.Web.Types.EmbedConstraint (EmbedConstraint)
import ZoomHub.Web.Types.EmbedDimension (EmbedDimension)
import ZoomHub.Web.Types.EmbedId (EmbedId, unEmbedId)
import ZoomHub.Web.Types.EmbedObjectFit (EmbedObjectFit)

-- API
type API =
  -- TODO: Route to homepage (`/`) using: `:<|> Get '[HTML]`
  -- Meta
  "health" :> Get '[HTML] String
    :<|> "version" :> Get '[HTML] String
    -- Config
    :<|> "internal"
      :> "config"
      :> Get '[JSON] API.Config
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
    :<|> "v1"
      :> "content"
      :> "upload"
      :> RequiredQueryParam "email" Text -- TODO: Introduce `Email` type
      :> Get '[JSON] (HashMap Text Text)
    -- API: Error: RESTful: Upload without email
    :<|> "v1"
      :> "content"
      :> "upload"
      :> Get '[JSON] (HashMap Text Text)
    -- API: RESTful: Reset
    :<|> Auth '[BasicAuth] Authentication.AuthenticatedUser
      :> "v1"
      :> "content"
      :> Capture "id" ContentId
      :> "reset"
      :> Put '[JSON] Content
    -- API: RESTful: Verification
    :<|> "v1"
      :> "content"
      :> Capture "id" ContentId
      :> "verification"
      :> Capture "token" VerificationToken
      :> Put '[JSON] Content
    -- API: RESTful: Error: Invalid verification token
    :<|> "v1"
      :> "content"
      :> Capture "id" ContentId
      :> "verification"
      :> Capture "token" String
      :> Put '[JSON] Content
    -- API: RESTful: Completion
    :<|> Auth '[BasicAuth] Authentication.AuthenticatedUser
      :> "v1"
      :> "content"
      :> Capture "id" ContentId
      :> "completion"
      :> ReqBody '[JSON] ContentCompletion
      :> Put '[JSON] Content
    -- API: RESTful: ID
    :<|> "v1" :> "content" :> Capture "id" ContentId :> Get '[JSON] Content
    -- API: RESTful: Error: ID
    :<|> "v1" :> "content" :> Capture "id" String :> Get '[JSON] Content
    -- API: RESTful: URL
    :<|> "v1" :> "content"
      :> RequiredQueryParam "url" ContentURI
      :> QueryParam "email" Text
      :> Get '[JSON] Content
    -- API: RESTful: Error: URL
    :<|> "v1" :> "content"
      :> QueryParam "url" String
      :> Get '[JSON] Content
    -- Web: Explore: Recent
    :<|> Auth '[BasicAuth] Authentication.AuthenticatedUser
      :> "explore"
      :> "recent"
      :> QueryParam "items" Int
      :> Get '[HTML] Page.ExploreRecentContent
    -- Web: Embed (iframe)
    :<|> Capture "embedId" ContentId
      :> "embed"
      :> QueryParam "fit" EmbedObjectFit
      :> QueryParam "constrain" EmbedConstraint
      :> QueryParam "background" EmbedBackground
      :> Get '[HTML] Page.EmbedContent
    -- Web: Embed (JavaScript)
    :<|> Capture "embedId" EmbedId
      :> QueryParam "id" String
      :> QueryParam "width" EmbedDimension
      :> QueryParam "height" EmbedDimension
      :> QueryParam "border" EmbedBorder
      :> QueryParam "fit" EmbedObjectFit
      :> QueryParam "constrain" EmbedConstraint
      :> QueryParam "background" EmbedBackground
      :> Get '[JavaScript] Embed
    -- Web: Verification
    :<|> Capture "viewId" ContentId
      :> "verify"
      :> Capture "token" VerificationToken
      :> Get '[HTML] Page.VerifyContent
    -- Web: View
    :<|> Capture "viewId" ContentId :> Get '[HTML] Page.ViewContent
    :<|> RequiredQueryParam "url" ContentURI :> Get '[HTML] Page.ViewContent
    -- Web: View: Error: Invalid URL
    :<|> RequiredQueryParam "url" String :> Get '[HTML] Page.ViewContent
    -- Web: Shortcut: `http://zoomhub.net/http://example.com`:
    :<|> RawCapture "viewURI" ContentURI :> Get '[HTML] Page.ViewContent
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
    :<|> restConfig config
    -- API: JSONP
    :<|> jsonpContentById baseURI contentBaseURI dbConnPool
    :<|> jsonpInvalidContentId
    :<|> jsonpContentByURL baseURI contentBaseURI dbConnPool
    :<|> jsonpInvalidRequest
    -- API: RESTful
    :<|> restUpload config awsConfig uploads
    :<|> restUploadWithoutEmail uploads
    :<|> restContentResetById baseURI dbConnPool
    :<|> restContentVerificationById baseURI dbConnPool
    :<|> restContentInvalidVerificationToken
    :<|> restContentCompletionById baseURI contentBaseURI dbConnPool
    :<|> restContentById baseURI contentBaseURI dbConnPool
    :<|> restInvalidContentId
    :<|> restContentByURL config baseURI dbConnPool processContent
    :<|> restInvalidRequest
    -- Web: Explore: Recent
    :<|> webExploreRecent baseURI contentBaseURI dbConnPool
    -- Web: Embed (iframe)
    :<|> webEmbedIFrame baseURI staticBaseURI contentBaseURI dbConnPool
    -- Web: Embed (JavaScript)
    :<|> webEmbed baseURI contentBaseURI staticBaseURI dbConnPool viewerScript
    -- Web: View
    :<|> webContentVerificationById baseURI contentBaseURI dbConnPool
    :<|> webContentById baseURI contentBaseURI (AWS.configSourcesS3Bucket awsConfig) dbConnPool
    :<|> webContentByURL baseURI dbConnPool
    :<|> webInvalidURLParam
    :<|> webContentByURL baseURI dbConnPool
    -- Web: Static files
    :<|> serveDirectory (Config.error404 config) publicPath
  where
    awsConfig = Config.aws config
    baseURI = Config.baseURI config
    contentBaseURI = Config.contentBaseURI config
    dbConnPool = Config.dbConnPool config
    processContent = Config.processContent config
    publicPath = Config.publicPath config
    staticBaseURI = Config.staticBaseURI config
    uploads = Config.uploads config
    viewerScript = Config.openSeadragonScript config

-- App
app :: Config -> IO Application
app config = do
  jwtKey <- generateKey
  return $ logger . simpleCors $ serveWithContext api (cfg jwtKey) (server config)
  where
    -- TODO: Can we use `BasicAuth` without JWT and cookies?
    cfg jwtKey =
      defaultJWTSettings jwtKey
        :. defaultCookieSettings
        :. Authentication.check (Config.apiUser config)
        :. EmptyContext
    logger = Config.logger config

-- Handlers

-- Meta
health :: Handler String
health = return "up"

version :: String -> Handler String
version = return

restConfig :: Config -> Handler API.Config
restConfig config =
  return
    API.Config
      { API.configEnvironment = Config.environment config,
        API.configBaseURI = Config.baseURI config,
        API.configUploadsEnabled = Config.uploads config == UploadsEnabled,
        API.configUploadsMaxSizeMegabytes = Config.maxUploadSizeMegabytes config
      }

-- API: JSONP
jsonpContentById ::
  BaseURI ->
  ContentBaseURI ->
  Pool Connection ->
  ContentId ->
  Callback ->
  Handler (JSONP (NonRESTfulResponse Content))
jsonpContentById baseURI contentBaseURI dbConnPool contentId callback = do
  maybeContent <- liftIO $ usingConnectionPool dbConnPool (PG.getById' contentId)
  case maybeContent of
    Nothing ->
      throwError $
        JSONP.mkError $
          mkJSONP callback $
            mkNonRESTful404 $
              contentNotFoundMessage contentId
    Just content -> do
      let publicContent = Content.fromInternal baseURI contentBaseURI content
      return $ mkJSONP callback $ mkNonRESTful200 "content" publicContent

jsonpContentByURL ::
  BaseURI ->
  ContentBaseURI ->
  Pool Connection ->
  ContentURI ->
  Callback ->
  Handler (JSONP (NonRESTfulResponse Content))
jsonpContentByURL baseURI contentBaseURI dbConnPool url callback = do
  maybeContent <- liftIO $ usingConnectionPool dbConnPool (PG.getByURL' url)
  case maybeContent of
    Nothing ->
      throwError . JSONP.mkError $
        mkJSONP callback (mkNonRESTful503 noNewContentErrorMessage)
    Just content -> do
      let publicContent = Content.fromInternal baseURI contentBaseURI content
          redirectLocation = apiRedirectURI baseURI contentId
          contentId = Internal.contentId content
      return $
        mkJSONP callback $
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

restUpload :: Config -> AWS.Config -> Uploads -> Text -> Handler (HashMap Text Text)
restUpload config awsConfig uploads email =
  case uploads of
    UploadsDisabled ->
      noNewContentErrorAPI
    UploadsEnabled -> do
      currentTime <- liftIO Time.getCurrentTime
      s3UploadKey <- T.pack . show <$> liftIO UUIDV4.nextRandom
      let expiryTime = Time.addUTCTime Time.nominalDay currentTime
          key = "uploads/" <> s3UploadKey
          s3BucketURL = "https://" <> s3Bucket <> ".s3.us-east-2.amazonaws.com"
          s3URL = s3BucketURL <> "/" <> key
          ePolicy =
            S3.newPostPolicy
              expiryTime
              [ S3.ppCondBucket s3Bucket,
                S3.ppCondKey key,
                S3.ppCondContentLengthRange minUploadSizeBytes maxUploadSizeBytes,
                S3.ppCondContentType "image/",
                PPCEquals
                  "success_action_redirect"
                  -- TODO: Use type-safe links
                  ( fold
                      [ T.pack . show . Config.baseURI $ config,
                        "/v1/content",
                        "?email=",
                        URIEncode.encodeText email,
                        "&url=",
                        URIEncode.encodeText s3URL
                      ]
                  )
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
              let connectInfo = setRegion "us-east-2" $ setCreds awsCredentials Minio.awsCI
              result <- liftIO $ runMinio connectInfo (S3.presignedPostPolicy policy)
              case result of
                Left minioErr ->
                  return $ HS.singleton "error" (T.pack $ show minioErr)
                Right (_url, formData) ->
                  return $ lenientDecodeUtf8 <$> HS.insert "url" (encodeUtf8 s3BucketURL) formData
      where
        minUploadSizeBytes = 1
        maxUploadSizeBytes =
          fromInteger $ Config.maxUploadSizeMegabytes config * 1024 * 1024
        s3Bucket = AWS.unS3BucketName . AWS.configSourcesS3Bucket $ awsConfig

restUploadWithoutEmail :: Uploads -> Handler (HashMap Text Text)
restUploadWithoutEmail UploadsDisabled = noNewContentErrorAPI
restUploadWithoutEmail UploadsEnabled = missingEmailErrorAPI

restContentResetById ::
  BaseURI ->
  Pool Connection ->
  AuthResult Authentication.AuthenticatedUser ->
  ContentId ->
  Handler Content
restContentResetById baseURI dbConnPool authResult contentId = do
  case authResult of
    Authenticated _ -> do
      maybeContent <-
        liftIO $
          usingConnectionPool
            dbConnPool
            (PG.unsafeResetAsInitializedWithVerification contentId)
      case maybeContent of
        Nothing ->
          throwError . API.error404 $ contentNotFoundMessage contentId
        Just content ->
          redirectToAPI baseURI $ Internal.contentId content
    NoSuchUser ->
      throwError . API.error401 $ "Invalid auth"
    BadPassword ->
      throwError . API.error401 $ "Invalid auth"
    Indefinite ->
      throwError . API.error401 $ "Missing 'Authorization' header"

restContentVerificationById ::
  BaseURI ->
  Pool Connection ->
  ContentId ->
  VerificationToken ->
  Handler Content
restContentVerificationById baseURI dbConnPool contentId verificationToken = do
  result <- liftIO $ usingConnectionPool dbConnPool (PG.markAsVerified contentId verificationToken)
  case result of
    Right content ->
      redirectToAPI baseURI $ Internal.contentId content
    Left VerificationError.TokenMismatch ->
      throwError . API.error401 $ "Unauthorized verification token: " <> show verificationToken
    Left VerificationError.ContentNotFound ->
      throwError . API.error404 $ contentNotFoundMessage contentId

restContentInvalidVerificationToken ::
  ContentId ->
  String -> -- VerificationToken
  Handler Content
restContentInvalidVerificationToken _contentId verificationToken =
  throwError . API.error401 $ "Invalid verification token: " <> verificationToken

restContentCompletionById ::
  BaseURI ->
  ContentBaseURI ->
  Pool Connection ->
  AuthResult Authentication.AuthenticatedUser ->
  ContentId ->
  ContentCompletion ->
  Handler Content
restContentCompletionById baseURI contentBaseURI dbConnPool authResult contentId completion =
  case authResult of
    Authenticated _ -> do
      maybeContent <- liftIO $
        usingConnectionPool dbConnPool $ case completion of
          Completion.Success sc ->
            PG.markAsSuccess
              contentId
              (DeepZoomImage.toInternal $ Completion.scDZI sc)
              (Completion.scMIME sc)
              (Just $ Completion.scSize sc)
          Completion.Failure fc ->
            PG.markAsFailure contentId (Just $ Completion.fcErrorMessage fc)
      case maybeContent of
        Nothing ->
          throwError . API.error404 $ contentNotFoundMessage contentId
        Just content ->
          return $ Content.fromInternal baseURI contentBaseURI content
    NoSuchUser ->
      throwError . API.error401 $ "Invalid auth"
    BadPassword ->
      throwError . API.error401 $ "Invalid auth"
    Indefinite ->
      throwError . API.error401 $ "Missing 'Authorization' header"

restContentById ::
  BaseURI ->
  ContentBaseURI ->
  Pool Connection ->
  ContentId ->
  Handler Content
restContentById baseURI contentBaseURI dbConnPool contentId = do
  maybeContent <- liftIO $ usingConnectionPool dbConnPool (PG.getById' contentId)
  case maybeContent of
    Nothing -> throwError . API.error404 $ contentNotFoundMessage contentId
    Just content -> return $ Content.fromInternal baseURI contentBaseURI content

restInvalidContentId :: String -> Handler Content
restInvalidContentId contentId =
  throwError . API.error404 $ noContentWithIdMessage contentId

restContentByURL ::
  Config ->
  BaseURI ->
  Pool Connection ->
  ProcessContent ->
  ContentURI ->
  Maybe Text -> -- Email
  Handler Content
restContentByURL config baseURI dbConnPool processContent url mEmail = do
  maybeContent <- liftIO $ usingConnectionPool dbConnPool (PG.getByURL' url)
  case (maybeContent, mEmail) of
    (Nothing, Nothing) ->
      missingEmailErrorAPI
    (Nothing, Just email) -> do
      mNewContent <- case processContent of
        ProcessExistingAndNewContent -> do
          mNewContent <- liftIO $ usingConnectionPool dbConnPool (PG.initialize url email)
          for_ mNewContent $ \newContent -> do
            case ( Internal.contentSubmitterEmail newContent,
                   Internal.contentVerificationToken newContent
                 ) of
              (Just submitterEmail, Just verificationToken) ->
                case environment of
                  Environment.Production ->
                    sendEmail (Internal.contentId newContent) submitterEmail verificationToken
                  Environment.Staging ->
                    sendEmail (Internal.contentId newContent) submitterEmail verificationToken
                  -- NOTE: Do not send email in test environment
                  Environment.Test ->
                    pure ()
                  Environment.Development ->
                    sendEmail (Internal.contentId newContent) submitterEmail verificationToken
              (mSubmitterEmail, mVerificationToken) -> do
                liftIO $
                  logWarning
                    "Cannot notify submitter due to missing email and/or verification token"
                    [ "contentId" .= Internal.contentId newContent,
                      "submitterEmail" .= mSubmitterEmail,
                      "verificationToken" .= mVerificationToken
                    ]
                pure ()
          -- TODO: Redirect to content:
          pure mNewContent
        ProcessExistingContent ->
          noNewContentErrorAPI
        ProcessNoContent ->
          noNewContentErrorAPI
      case mNewContent of
        Just newContent ->
          redirectToAPI baseURI (Internal.contentId newContent)
        Nothing ->
          throwError . API.error503 $ failedToCreateContentErrorMessage
    (Just content, _) ->
      redirectToAPI baseURI (Internal.contentId content)
  where
    logLevel = Config.logLevel config
    awsConfig = Config.aws config
    environment = Config.environment config
    sendEmail contentId submitterEmail verificationToken =
      void . liftIO $
        Email.send awsConfig logLevel $
          Verification.request
            baseURI
            contentId
            verificationToken
            (Email.From "\"ZoomHub\" <daniel@zoomhub.net>")
            (Email.To submitterEmail)

restInvalidRequest :: Maybe String -> Handler Content
restInvalidRequest maybeURL = case maybeURL of
  Nothing -> throwError . API.error400 $ apiMissingIdOrURLMessage
  Just _ -> throwError . API.error400 $ invalidURLErrorMessage

-- Web: Explore: Recent
webExploreRecent ::
  BaseURI ->
  ContentBaseURI ->
  Pool Connection ->
  AuthResult Authentication.AuthenticatedUser ->
  Maybe Int ->
  Handler Page.ExploreRecentContent
webExploreRecent baseURI contentBaseURI dbConnPool authResult mNumItems =
  case authResult of
    Authenticated _ -> do
      let minItems = 1
          maxItems = 200
          numItems = fromMaybe 50 mNumItems
      when (numItems > maxItems || numItems < minItems) $
        throwError . Web.error400 $
          "'numItems' must be between "
            <> show minItems
            <> " and "
            <> show maxItems
            <> ": "
            <> show numItems
      content <- liftIO $ usingConnectionPool dbConnPool (PG.getRecent (fromIntegral numItems))
      return
        Page.ExploreRecentContent
          { ercContent = content,
            ercBaseURI = baseURI,
            ercContentBaseURI = contentBaseURI
          }
    NoSuchUser ->
      throwError . Web.error401 $ "Invalid auth"
    BadPassword ->
      throwError . Web.error401 $ "Invalid auth"
    Indefinite ->
      throwError $ wwwAuthenticatedErr "ZoomHub"

-- Web: Embed
webEmbedIFrame ::
  BaseURI ->
  StaticBaseURI ->
  ContentBaseURI ->
  Pool Connection ->
  ContentId ->
  Maybe EmbedObjectFit ->
  Maybe EmbedConstraint ->
  Maybe EmbedBackground ->
  Handler Page.EmbedContent
webEmbedIFrame
  baseURI
  staticBaseURI
  contentBaseURI
  dbConnPool
  contentId
  mObjectFit
  mEmbedConstraint
  mEmbedBackground = do
    maybeContent <- liftIO $ usingConnectionPool dbConnPool (PG.getById contentId)
    case maybeContent of
      Nothing ->
        throwError . Web.error404 $ contentNotFoundMessage contentId
      Just c -> do
        let content = Content.fromInternal baseURI contentBaseURI c
        return $
          Page.EmbedContent
            { ecBackgroundColor = mEmbedBackground,
              ecBaseURI = baseURI,
              ecConstraint = mEmbedConstraint,
              ecContent = content,
              ecObjectFit = mObjectFit,
              ecStaticBaseURI = staticBaseURI
            }

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
  Maybe EmbedBorder ->
  Maybe EmbedObjectFit ->
  Maybe EmbedConstraint ->
  Maybe EmbedBackground ->
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
  height
  border
  objectFit
  constraint
  backgroundColor = do
    maybeContent <- liftIO $ usingConnectionPool dbConnPool (PG.getById' contentId)
    case maybeContent of
      Nothing ->
        throwError . Web.error404 $ contentNotFoundMessage contentId
      Just content -> do
        let randomIdRange = (100000, 999999) :: (Int, Int)
        randomId <- liftIO $ randomRIO randomIdRange
        let containerId = fromMaybe (defaultContainerId randomId) maybeId
            publicContent = Content.fromInternal baseURI contentBaseURI content
        return $
          Embed
            { embedBaseURI = baseURI,
              embedStaticBaseURI = staticBaseURI,
              embedContainerId = containerId,
              embedContent = publicContent,
              embedBody = viewerScript,
              embedWidth = width,
              embedHeight = height,
              embedBorder = border,
              embedObjectFit = objectFit,
              embedConstraint = constraint,
              embedBackgroundColor = backgroundColor
            }
    where
      contentId = unEmbedId embedId
      defaultContainerId n = "zoomhub-embed-" ++ show n

-- Web: Verification
-- TODO: Refactor to call API or extract implementation from API:
webContentVerificationById ::
  BaseURI ->
  ContentBaseURI ->
  Pool Connection ->
  ContentId ->
  VerificationToken ->
  Handler Page.VerifyContent
webContentVerificationById baseURI contentBaseURI dbConnPool contentId verificationToken = do
  result <- liftIO $ usingConnectionPool dbConnPool (PG.markAsVerified contentId verificationToken)
  case result of
    Right internalContent | ContentState.isCompleted (Internal.contentState internalContent) -> do
      redirectToView baseURI (Internal.contentId internalContent)
    Right internalContent -> do
      let content = Content.fromInternal baseURI contentBaseURI internalContent
      return $ Page.mkVerifyContent baseURI (VerificationResult.Success content)
    Left VerificationError.TokenMismatch ->
      return $ Page.mkVerifyContent baseURI (VerificationResult.Error "Cannot verify submission :(")
    Left VerificationError.ContentNotFound ->
      throwError . Web.error404 $ contentNotFoundMessage contentId

-- Web: View
webContentById ::
  BaseURI ->
  ContentBaseURI ->
  AWS.S3BucketName ->
  Pool Connection ->
  ContentId ->
  Handler Page.ViewContent
webContentById baseURI contentBaseURI awsSourcesS3BucketName dbConnPool contentId = do
  maybeContent <- liftIO $ usingConnectionPool dbConnPool (PG.getById contentId)
  case maybeContent of
    Nothing ->
      throwError . Web.error404 $ contentNotFoundMessage contentId
    Just c -> do
      let content = Content.fromInternal baseURI contentBaseURI c
      return $
        ViewContent
          { vcBaseURI = baseURI,
            vcContent = content,
            vcAWSSourcesS3BucketName = awsSourcesS3BucketName
          }

-- TODO: Add support for submission, i.e. create content in the background:
webContentByURL ::
  BaseURI ->
  Pool Connection ->
  ContentURI ->
  Handler Page.ViewContent
webContentByURL baseURI dbConnPool contentURI = do
  maybeContent <- liftIO $ usingConnectionPool dbConnPool (PG.getByURL contentURI)
  case maybeContent of
    Nothing ->
      noNewContentErrorWeb
    Just content ->
      redirectToView baseURI (Internal.contentId content)

webInvalidURLParam :: String -> Handler Page.ViewContent
webInvalidURLParam _ = throwError . Web.error400 $ invalidURLErrorMessage

-- Helpers
contentNotFoundMessage :: ContentId -> String
contentNotFoundMessage contentId =
  noContentWithIdMessage (unContentId contentId)

noContentWithIdMessage :: String -> String
noContentWithIdMessage contentId = "No content with ID: " ++ contentId

noNewContentErrorWeb :: Handler Page.ViewContent
noNewContentErrorWeb = noNewContentError Web.error503

noNewContentErrorAPI :: Handler a
noNewContentErrorAPI = noNewContentError API.error503

noNewContentError :: (String -> ServerError) -> Handler a
noNewContentError err =
  throwError . err $ noNewContentErrorMessage

noNewContentErrorMessage :: String
noNewContentErrorMessage = "We are currently not processing new content."

missingEmailErrorAPI :: Handler a
missingEmailErrorAPI =
  throwError . API.error400 $
    "ZoomHub now requires an 'email' query parameter to know who submitted an image."

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

redirectToView :: BaseURI -> ContentId -> Handler a
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
  -- HACK: Redirect using error: http://git.io/vBCz9
  throwError $ err301 {errHeaders = [("Location", BC.pack (show location))]}
