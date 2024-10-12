{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ZoomHub.API
  ( app,
  )
where

import qualified Amazonka
import Control.Monad (void, when)
import Control.Monad.Except (throwError)
import Control.Monad.IO.Class (liftIO)
import qualified Crypto.JWT as JWT
import Data.Aeson ((.=))
import qualified Data.Aeson as JSON
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (fold, for_)
import Data.Functor ((<&>))
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HS
import Data.Maybe (fromJust, fromMaybe)
import Data.Proxy (Proxy (Proxy))
import qualified Data.String as String
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Time as Time
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUIDV4
import Flow
import Network.OAuth2.Experiment (mkAuthorizationRequest)
import qualified Network.OAuth2.Experiment as NOA2
import Network.URI (URI, parseRelativeReference, relativeTo)
import qualified Network.URI.Encode as URIEncode
import Network.Wai (Application, Request (requestHeaders))
import Network.Wai.Middleware.Cors (simpleCors)
import Servant
  ( Capture,
    Context (EmptyContext, (:.)),
    Get,
    Handler,
    Header,
    Headers,
    JSON,
    NoContent (..),
    PlainText,
    Put,
    QueryParam,
    Raw,
    ReqBody,
    Server,
    ServerError (errBody),
    ToHttpApiData (toUrlPiece),
    addHeader,
    err301,
    err401,
    errHeaders,
    serveWithContext,
    (:<|>) (..),
    (:>),
  )
import Servant.API.Experimental.Auth (AuthProtect)
import Servant.API.Verbs (StdMethod (GET), Verb)
import Servant.Auth.Server
  ( Auth,
    AuthResult (Authenticated, BadPassword, Indefinite, NoSuchUser),
    BasicAuth,
    defaultCookieSettings,
    defaultJWTSettings,
    wwwAuthenticatedErr,
  )
import qualified Servant.Auth.Server as Auth
import Servant.HTML.Lucid (HTML)
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler)
import Squeal.PostgreSQL.Session.Pool (Pool, usingConnectionPool)
import System.Random (randomRIO)
import URI.ByteString.Instances ()
import Web.ClientSession (Key)
import qualified Web.ClientSession as ClientSession
import Web.Cookie
  ( CookiesText,
    SetCookie (..),
    parseCookiesText,
  )
import ZoomHub.API.ContentTypes.JavaScript (JavaScript)
import qualified ZoomHub.API.Cookie as API
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
import qualified ZoomHub.AWS.S3 as S3
import qualified ZoomHub.AWS.S3.POSTPolicy as S3
import qualified ZoomHub.AWS.S3.POSTPolicy.Condition as POSTPolicyCondition
import qualified ZoomHub.Authentication.Basic as BasicAuthentication
import qualified ZoomHub.Authentication.Cookie as Cookie
import ZoomHub.Authentication.OAuth (AuthorizeState (..), IdToken (..), State (unState), generateState)
import qualified ZoomHub.Authentication.OAuth as OAuth
import ZoomHub.Authentication.OAuth.Kinde (fetchTokensFor, mkApp, mkIdp)
import qualified ZoomHub.Authentication.OAuth.Kinde as Kinde
import qualified ZoomHub.Authentication.OAuth.Kinde.OAuth2CodeExchangeResponse as OAuth2CodeExchangeResponse
import ZoomHub.Authentication.OAuth.Kinde.Prompt (Prompt)
import qualified ZoomHub.Authentication.OAuth.Kinde.Prompt as Prompt
import ZoomHub.Authentication.Session (DecodedIdToken (..), Session (..))
import qualified ZoomHub.Authentication.Session as Session
import qualified ZoomHub.Authentication.Session as User
import ZoomHub.Config (Config)
import qualified ZoomHub.Config as Config
import qualified ZoomHub.Config.AWS as AWS
import qualified ZoomHub.Config.Kinde as Kinde
import ZoomHub.Config.ProcessContent (ProcessContent (..))
import ZoomHub.Config.Uploads (Uploads (..))
import qualified ZoomHub.Email as Email
import qualified ZoomHub.Email.Verification as Verification
import ZoomHub.Log.Logger (logWarning)
import ZoomHub.Servant.RequiredQueryParam (RequiredQueryParam)
import ZoomHub.Storage.PostgreSQL as PG
import ZoomHub.Storage.PostgreSQL.Dashboard as PG
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
import ZoomHub.Utils (appendQueryParams, tshow)
import qualified ZoomHub.Web.Errors as Web
import qualified ZoomHub.Web.Page.Dashboard as Page
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
  -- Meta
  "health" :> Get '[PlainText] Text
    :<|> "version" :> Get '[PlainText] Text
    -- Config
    :<|> "internal"
      :> "config"
      :> Get '[JSON] API.Config
    -- JSONP: ID
    :<|> "v1"
      :> "content"
      :> Capture "id" ContentId
      :> RequiredQueryParam "callback" Callback
      :> Get '[JavaScript] (JSONP (NonRESTfulResponse Content))
    -- JSONP: Error: ID
    :<|> "v1"
      :> "content"
      :> Capture "id" String
      :> RequiredQueryParam "callback" Callback
      :> Get '[JavaScript] (JSONP (NonRESTfulResponse String))
    -- JSONP: URL
    :<|> "v1"
      :> "content"
      :> RequiredQueryParam "url" ContentURI
      :> RequiredQueryParam "callback" Callback
      :> Get '[JavaScript] (JSONP (NonRESTfulResponse Content))
    -- JSONP: Error: URL
    :<|> "v1"
      :> "content"
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
    :<|> Auth '[BasicAuth] BasicAuthentication.AuthenticatedUser
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
    :<|> Auth '[BasicAuth] BasicAuthentication.AuthenticatedUser
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
    :<|> "v1"
      :> "content"
      :> RequiredQueryParam "url" ContentURI
      :> QueryParam "email" Text
      :> Get '[JSON] Content
    -- API: RESTful: Error: URL
    :<|> "v1"
      :> "content"
      :> QueryParam "url" String
      :> Get '[JSON] Content
    -- Web: Auth
    :<|> AuthProtect "cookie-session"
      :> "dashboard"
      :> Get '[HTML] Page.Dashboard
    -- Web: Auth
    :<|> "register" :> Verb 'GET 302 '[HTML] SetCookieAndRedirect
    :<|> "login" :> Verb 'GET 302 '[HTML] SetCookieAndRedirect
    :<|> "logout" :> Verb 'GET 302 '[HTML] SetCookieAndRedirect
    :<|> "auth"
      :> "kinde"
      :> "callback"
      :> Header "Cookie" Cookie.Header
      :> RequiredQueryParam "code" OAuth.AuthorizationCode
      :> RequiredQueryParam "state" OAuth.State
      :> QueryParam "scope" OAuth.Scope
      :> Verb 'GET 302 '[HTML] KindeCallback
    :<|> Auth '[BasicAuth] BasicAuthentication.AuthenticatedUser
      :> AuthProtect "cookie-session"
      :> "auth"
      :> "session"
      :> "debug"
      :> Get '[PlainText] Text
    -- Web: Explore: Recent
    :<|> Auth '[BasicAuth] BasicAuthentication.AuthenticatedUser
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
    -- Web: Dashboard
    :<|> webDashboard dbConnPool
    -- Web: Auth
    :<|> webRegister config.kinde config.clientSessionKey
    :<|> webLogin config.kinde config.clientSessionKey
    :<|> webLogout config.kinde
    :<|> webAuthKindeCallback config.clientSessionKey config.kinde
    :<|> webAuthSessionDebug
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
  jwtKey <- Auth.generateKey
  return $ logger . simpleCors $ serveWithContext api (context jwtKey) (server config)
  where
    -- TODO: Can we use `BasicAuth` without JWT and cookies?
    context jwtKey =
      defaultJWTSettings jwtKey
        :. defaultCookieSettings
        :. BasicAuthentication.check (Config.apiUser config)
        :. sessionAuthHandler config.clientSessionKey
        :. EmptyContext
    logger = Config.logger config

-- Handlers

-- Meta
health :: Handler Text
health = return "up"

version :: Text -> Handler Text
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
      s3UploadKey <- UUID.toText <$> liftIO UUIDV4.nextRandom
      let expiryTime = Time.addUTCTime Time.nominalDay currentTime
          key = "uploads/" <> s3UploadKey
          s3Region = AWS.configRegion awsConfig
          s3BucketURL = "https://" <> s3Bucket <> ".s3." <> Amazonka.fromRegion s3Region <> ".amazonaws.com"
          s3URL = s3BucketURL <> "/" <> key
          ePolicy =
            S3.mkPOSTPolicy
              expiryTime
              [ POSTPolicyCondition.bucket s3Bucket,
                POSTPolicyCondition.key key,
                POSTPolicyCondition.contentLengthRange
                  minUploadSizeBytes
                  maxUploadSizeBytes,
                POSTPolicyCondition.contentType "image/",
                POSTPolicyCondition.Equals
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
          formData <-
            liftIO $
              HS.map T.decodeUtf8Lenient
                <$> S3.presignPOSTPolicy
                  (AWS.configAccessKeyId awsConfig)
                  (AWS.configSecretAccessKey awsConfig)
                  (AWS.configRegion awsConfig)
                  policy
          return $ HS.insert "url" s3BucketURL formData
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
  AuthResult BasicAuthentication.AuthenticatedUser ->
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
  AuthResult BasicAuthentication.AuthenticatedUser ->
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

-- Web: Auth: Kinde

type SetCookieAndRedirect =
  Headers '[Header "Set-Cookie" SetCookie, Header "Location" Text] NoContent

type KindeCallback =
  Headers
    '[ Header "Location" Text,
       Header "Set-Cookie" SetCookie,
       Header "Set-Cookie" SetCookie
     ]
    NoContent

webRegister :: Kinde.Config -> Key -> Handler SetCookieAndRedirect
webRegister = webAuthRedirect Prompt.Create

webLogin :: Kinde.Config -> Key -> Handler SetCookieAndRedirect
webLogin = webAuthRedirect Prompt.Login

webAuthRedirect :: Prompt -> Kinde.Config -> Key -> Handler SetCookieAndRedirect
webAuthRedirect prompt kindeConfig clientSessionKey = do
  state <- liftIO generateState
  setCookieHeader <- liftIO $ API.oauth2StateCookieHeader clientSessionKey state
  let authorizationRedirectURI =
        mkAuthorizationRequest $ mkApp kindeConfig (unAuthorizeState state)
  return $
    addHeader setCookieHeader $
      addHeader
        ( toUrlPiece
            ( authorizationRedirectURI
                |> appendQueryParams [("prompt", Prompt.toByteString prompt)]
            )
        )
        NoContent

webLogout :: Kinde.Config -> Handler SetCookieAndRedirect
webLogout kindeConfig = do
  let mLogoutRedirectURI = Kinde.logoutURI kindeConfig
  case mLogoutRedirectURI of
    Just logoutRedirectURI ->
      return $
        addHeader (Cookie.empty API.sessionCookieName) $
          addHeader (toUrlPiece logoutRedirectURI) NoContent
    Nothing ->
      throwError $ Web.error503 "Invalid logout URL"

webAuthKindeCallback ::
  Key ->
  Kinde.Config ->
  Maybe Cookie.Header ->
  OAuth.AuthorizationCode ->
  OAuth.State ->
  Maybe OAuth.Scope ->
  Handler KindeCallback
webAuthKindeCallback clientSessionKey kindeConfig mCookieHeader code state _scope = do
  let mExpectedState = do
        cookies <- mCookieHeader <&> (parseCookiesText . T.encodeUtf8 . Cookie.unHeader)
        Cookie.value clientSessionKey API.oauth2StateCookieName cookies
      actualState = state |> unState |> TL.fromStrict |> NOA2.AuthorizeState |> AuthorizeState
  eResponse <-
    case mExpectedState of
      Just expectedState | expectedState == actualState -> do
        mTokens <- liftIO $ fetchTokensFor (mkIdp kindeConfig.domain) kindeConfig code
        case mTokens of
          Just tokens' ->
            pure $ Right tokens'
          Nothing ->
            pure $ Left ("Failed to fetch tokens." :: Text)
      Just _ ->
        pure $ Left "OAuth2 state does not match."
      Nothing ->
        pure $ Left "OAuth2 state is missing."
  eSession <- do
    case eResponse of
      Left message -> pure $ Left message
      Right response ->
        let jwtBS = response.idToken |> unIdToken |> TL.fromStrict |> TL.encodeUtf8
         in case JWT.decodeCompact @JWT.SignedJWT jwtBS :: Either JWT.JWTError JWT.SignedJWT of
              Left jwtError ->
                return $ Left $ "Failed to decode JWT: " <> tshow jwtError
              Right signedJWT -> do
                jwtResult <- liftIO $ verifyJWT kindeConfig.jwk signedJWT
                case jwtResult of
                  Left jwtError ->
                    return $ Left $ "Failed to verify JWT: " <> tshow jwtError
                  Right decodedIdToken ->
                    return $
                      Right
                        Session
                          { currentUser = decodedIdToken.user,
                            accessToken = response.accessToken,
                            refreshToken = response.refreshToken
                          }
  sessionSetCookieHeader <- case eSession of
    Left _ -> pure $ Cookie.empty API.sessionCookieName
    Right session -> liftIO $ API.sessionCookieHeader clientSessionKey session
  let redirectPath = case eSession of
        Left message -> "/?errorMessage=" <> (message |> URIEncode.encodeText)
        Right _ -> "/auth/session/debug"
  return $
    addHeader redirectPath $
      addHeader (Cookie.empty API.oauth2StateCookieName) $
        addHeader sessionSetCookieHeader NoContent
  where
    verifyJWT :: JWT.JWK -> JWT.SignedJWT -> IO (Either JWT.JWTError DecodedIdToken)
    verifyJWT jwk jwt = JWT.runJOSE $ do
      -- NOTE: The `aud` parameter used to be the URL but now it’s only the
      -- client ID:
      let aud =
            kindeConfig
              |> Kinde.clientId
              |> Kinde.unClientId
              |> T.unpack
              |> String.fromString
      let config = JWT.defaultJWTValidationSettings (== aud)
      JWT.verifyJWT config jwk jwt

webDashboard :: Pool Connection -> Maybe Session -> Handler Page.Dashboard
webDashboard dbConnPool mSession = case mSession of
  Just session -> do
    content <- liftIO $ usingConnectionPool dbConnPool (PG.getByEmail (session |> Session.currentUser |> User.email))
    return $
      Page.Dashboard
        { Page.dbSession = session,
          Page.dbContent = content
        }
  Nothing ->
    throwError . Web.error401 $ "No session"

webAuthSessionDebug ::
  AuthResult BasicAuthentication.AuthenticatedUser -> Maybe Session -> Handler Text
webAuthSessionDebug authResult mSession =
  case authResult of
    Authenticated _ -> do
      case mSession of
        Just session ->
          return (session |> JSON.encode |> BL.toStrict |> T.decodeUtf8Lenient)
        Nothing -> return "(no session)"
    NoSuchUser ->
      throwError . Web.error401 $ "Invalid auth"
    BadPassword ->
      throwError . Web.error401 $ "Invalid auth"
    Indefinite ->
      throwError $ wwwAuthenticatedErr "ZoomHub"

-- Web: Explore: Recent
webExploreRecent ::
  BaseURI ->
  ContentBaseURI ->
  Pool Connection ->
  AuthResult BasicAuthentication.AuthenticatedUser ->
  Maybe Int ->
  Handler Page.ExploreRecentContent
webExploreRecent baseURI contentBaseURI dbConnPool authResult mNumItems =
  case authResult of
    Authenticated _ -> do
      let minItems = 1
          maxItems = 500
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
      defaultContainerId n = "zoomhub-embed-" <> show n

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
noContentWithIdMessage contentId = "No content with ID: " <> contentId

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
  (fromJust . parseRelativeReference $ pathPrefix <> unContentId contentId)
    `relativeTo` unBaseURI baseURI

-- NOTE: Enable Chrome developer console ‘[x] Disable cache’ to test
-- permanent HTTP 301 redirects:
redirect :: URI -> Handler a
redirect location =
  -- HACK: Redirect using error: http://git.io/vBCz9
  throwError $ err301 {errHeaders = [("Location", BC.pack (show location))]}

-- Authentication using session cookie
-- Based on https://docs.servant.dev/en/stable/tutorial/Authentication.html
decodeSession :: ClientSession.Key -> CookiesText -> Handler (Maybe Session)
decodeSession key cookiesText =
  return $ Cookie.value key API.sessionCookieName cookiesText

sessionAuthHandler :: ClientSession.Key -> AuthHandler Request (Maybe Session)
sessionAuthHandler clientSessionKey = mkAuthHandler handler
  where
    maybeToEither e = maybe (Left e) Right
    throw401 msg = throwError $ err401 {errBody = msg}
    handler req = either throw401 (decodeSession clientSessionKey) $ do
      cookie <-
        maybeToEither "Missing cookie header" $
          lookup "cookie" $
            requestHeaders req
      Right $ parseCookiesText cookie

-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "cookie-session") = Maybe Session
