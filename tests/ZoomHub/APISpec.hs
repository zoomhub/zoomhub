{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module ZoomHub.APISpec
  ( main,
    spec,
  )
where

import Control.Concurrent (getNumCapabilities)
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (fromJust)
import Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Encoding.Base64 as T
import Data.Time.Units (Second)
import Network.HTTP.Types (hAuthorization, hContentType, methodGet, methodPut)
import Network.URI (URI, parseURIReference)
import Network.Wai (Middleware)
import Squeal.PostgreSQL.Pool (runPoolPQ)
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec (Spec, afterAll_, context, describe, hspec, it, shouldBe)
import Test.Hspec.Wai
  ( MatchHeader,
    ResponseMatcher,
    get,
    liftIO,
    matchHeaders,
    matchStatus,
    post,
    put,
    request,
    shouldRespondWith,
    with,
    (<:>),
  )
import Text.RawString.QQ (r)
import ZoomHub.API (app)
import ZoomHub.Config (Config (..))
import qualified ZoomHub.Config as Config
import ZoomHub.Config.ProcessContent (ProcessContent (ProcessExistingAndNewContent))
import ZoomHub.Config.Uploads (Uploads (UploadsDisabled))
import ZoomHub.Storage.PostgreSQL (createConnectionPool, getById)
import qualified ZoomHub.Storage.PostgreSQL as ConnectInfo (fromEnv)
import ZoomHub.Storage.PostgreSQL.Internal (destroyAllResources)
import ZoomHub.Types.APIUser (APIUser (..))
import ZoomHub.Types.BaseURI (BaseURI (BaseURI))
import ZoomHub.Types.Content (contentNumViews, contentSubmitterEmail, contentVerificationToken)
import ZoomHub.Types.ContentBaseURI (mkContentBaseURI)
import ZoomHub.Types.ContentId (ContentId, unContentId)
import qualified ZoomHub.Types.ContentId as ContentId
import ZoomHub.Types.StaticBaseURI (StaticBaseURI (StaticBaseURI))
import ZoomHub.Types.TempPath (TempPath (TempPath))

main :: IO ()
main = hspec spec

-- Helpers
toURI :: String -> URI
toURI s =
  case parseURIReference s of
    Just uri -> uri
    _ -> error $ "ZoomHub.APISpec.toURI: Failed to parse URI: " ++ s

existingContent :: (ContentId, String)
existingContent =
  ( ContentId.fromString "yQ4",
    "http://media.stenaline.com/media_SE/lalandia-map-zoomit/lalandia-map.jpg"
  )

-- Matchers
applicationJSON :: MatchHeader
applicationJSON = "Content-Type" <:> "application/json;charset=utf-8"

plainTextUTF8 :: MatchHeader
plainTextUTF8 = "Content-Type" <:> "text/plain; charset=utf-8"

plainText :: MatchHeader
plainText = "Content-Type" <:> "text/plain"

javaScriptUTF8 :: MatchHeader
javaScriptUTF8 = "Content-Type" <:> "application/javascript;charset=utf-8"

invalidURL :: ResponseMatcher
invalidURL =
  "Please give us the full URL, including ‘http://’ or ‘https://’."
    { matchStatus = 400,
      matchHeaders = [plainTextUTF8]
    }

invalidHTTPMethod :: ResponseMatcher
invalidHTTPMethod =
  "Only GET or HEAD is supported"
    { matchStatus = 405,
      matchHeaders = [plainText]
    }

noNewContent :: ResponseMatcher
noNewContent =
  "We are currently not processing new content."
    { matchStatus = 503,
      matchHeaders = [plainTextUTF8]
    }

restRedirect :: ContentId -> ResponseMatcher
restRedirect cId =
  ""
    { matchStatus = 301,
      matchHeaders = ["Location" <:> BC.pack expectedLocation]
    }
  where
    baseURIPrefix = show . Config.baseURI $ config
    expectedLocation = baseURIPrefix ++ "/v1/content/" ++ unContentId cId

viewRedirect :: ContentId -> ResponseMatcher
viewRedirect cId =
  ""
    { matchStatus = 301,
      matchHeaders = ["Location" <:> BC.pack expectedLocation]
    }
  where
    baseURIPrefix = show . Config.baseURI $ config
    expectedLocation = baseURIPrefix <> "/" <> unContentId cId

-- Config
nullLogger :: Middleware
nullLogger = id

newContentId :: String
newContentId = "Xar"

newContentURL :: String
newContentURL = "http://example.com"

testEmail :: String
testEmail = "test@example.com"

authorizedUser :: APIUser
authorizedUser = APIUser {username = "worker", password = "secr3t"}

{-# NOINLINE config #-}
config :: Config
config =
  Config
    { apiUser = authorizedUser,
      aws = undefined, -- TODO: Test AWS functionality
      baseURI = BaseURI (toURI "http://localhost:8000"),
      contentBaseURI = case mkContentBaseURI (toURI "http://localhost:9000/_dzis_") of
        Just uri -> uri
        _ -> error "ZoomHub.APISpec: Failed to parse `Config.contentBaseURI`.",
      dbConnInfo = dbConnInfo',
      dbConnPool = dbConnPool',
      dbConnPoolIdleTime = dbConnPoolIdleTime',
      dbConnPoolMaxResourcesPerStripe = dbConnPoolMaxResourcesPerStripe',
      dbConnPoolNumStripes = dbConnPoolNumStripes',
      error404 = "404",
      logger = nullLogger,
      openSeadragonScript = "osd",
      port = 8000,
      processContent = ProcessExistingAndNewContent,
      publicPath = "./public",
      staticBaseURI = StaticBaseURI (toURI "https://static.zoomhub.net"),
      tempPath = TempPath "./data/temp",
      uploads = UploadsDisabled,
      version = "test"
    }
  where
    numSpindles = 1
    -- TODO: How can we avoid `unsafePerformIO`?
    numCapabilities = fromIntegral $ unsafePerformIO getNumCapabilities
    -- TODO: How can we avoid `unsafePerformIO`?
    dbConnInfo' = unsafePerformIO $ ConnectInfo.fromEnv "zoomhub_test"
    -- TODO: How can we avoid `unsafePerformIO`?
    dbConnPool' =
      unsafePerformIO $
        createConnectionPool
          dbConnInfo'
          dbConnPoolNumStripes'
          dbConnPoolIdleTime'
          dbConnPoolMaxResourcesPerStripe'
    dbConnPoolIdleTime' = 10 :: Second
    dbConnPoolMaxResourcesPerStripe' = (numCapabilities * 2) + numSpindles
    dbConnPoolNumStripes' = 1

closeDatabaseConnection :: Config -> IO ()
closeDatabaseConnection = destroyAllResources . dbConnPool

spec :: Spec
spec = with (app config) $ afterAll_ (closeDatabaseConnection config) do
  describe "RESTful" do
    describe "Upload (GET /v1/content/upload)" do
      it "should return  503" $
        get "/v1/content/upload" `shouldRespondWith` noNewContent
    describe "List (GET /v1/content)" do
      it "should be interpreted as a ‘get by URL’, with no URL given" $
        get "/v1/content"
          `shouldRespondWith` [r|Missing ID or URL. Please provide ID, e.g. `/v1/content/<id>`, or URL via `/v1/content?url=<url>` query parameter.|]
            { matchStatus = 400,
              matchHeaders = [plainTextUTF8]
            }
    describe "Get by URL (GET /v1/content?url=…)" do
      it "should reject empty URLs" $
        get "/v1/content?url="
          `shouldRespondWith` invalidURL
      it "should reject malformed URLs" $
        get "/v1/content?url=lasjdoasj)(¨‚Ô‚ˆÔ∏ŒÂ;sd)"
          `shouldRespondWith` invalidURL
      it "should reject URLs without protocol" $
        get "/v1/content?url=example.com"
          `shouldRespondWith` invalidURL
      it "should reject URLs with non-HTTP protocol" do
        get "/v1/content?url=ftp://example.com"
          `shouldRespondWith` invalidURL
        get "/v1/content?url=mailto://example@example.com"
          `shouldRespondWith` invalidURL
      it "should accept new HTTP URLs" do
        get ("/v1/content?email=" <> BC.pack testEmail <> "&url=" <> BC.pack newContentURL)
          `shouldRespondWith` restRedirect (ContentId.fromString newContentId)
        liftIO do
          let pool = Config.dbConnPool config
          mContent <- runPoolPQ (getById $ ContentId.fromString newContentId) pool
          (mContent >>= contentSubmitterEmail) `shouldBe` (Just $ T.pack testEmail)
          (mContent >>= \c -> fromIntegral . length . show <$> contentVerificationToken c) `shouldBe` (Just (36 :: Integer))
        get ("/v1/content/" <> BC.pack newContentId)
          `shouldRespondWith` [r|{"dzi":null,"progress":0,"url":"http://example.com","verified":false,"embedHtml":"<script src=\"http://localhost:8000/Xar.js?width=auto&height=400px\"></script>","shareUrl":"http://localhost:8000/Xar","id":"Xar","ready":false,"failed":false}|]
            { matchStatus = 200,
              matchHeaders = [applicationJSON]
            }
      it "should redirect existing (converted) HTTP URLs to ID" $
        let (existingId, existingURL) = existingContent
         in get ("/v1/content?url=" <> BC.pack existingURL)
              `shouldRespondWith` restRedirect existingId
    describe "Get by ID (GET /v1/content/:id)" do
      it "should return correct data for existing content" $
        get "/v1/content/yQ4"
          `shouldRespondWith` [r|{"dzi":{"height":3750,"url":"http://localhost:9000/_dzis_/yQ4.dzi","width":5058,"tileOverlap":1,"tileFormat":"jpg","tileSize":254},"progress":1,"url":"http://media.stenaline.com/media_SE/lalandia-map-zoomit/lalandia-map.jpg","verified":false,"embedHtml":"<script src=\"http://localhost:8000/yQ4.js?width=auto&height=400px\"></script>","shareUrl":"http://localhost:8000/yQ4","id":"yQ4","ready":true,"failed":false}|]
            { matchStatus = 200,
              matchHeaders = [applicationJSON]
            }
      it "should return 404 non-existent content" $
        get "/v1/content/nonExistentContent"
          `shouldRespondWith` "No content with ID: nonExistentContent"
            { matchStatus = 404,
              matchHeaders = [plainTextUTF8]
            }
    describe "Verify content by ID (GET /v1/content/:id/verification/:token)" do
      it "should return 404 non-existent content" $
        get "/v1/content/nonExistentContent/verification/00000000-0000-0000-0000-000000000000"
          `shouldRespondWith` "No content with ID: nonExistentContent"
            { matchStatus = 404,
              matchHeaders = [plainTextUTF8]
            }
    it "should return 401 invalid verification token" $
      get "/v1/content/yQ4/verification/invalid-token"
        `shouldRespondWith` "Invalid verification token: invalid-token"
          { matchStatus = 401,
            matchHeaders = [plainTextUTF8]
          }
    it "should verify content" do
      maybeContent <- liftIO $ runPoolPQ (getById $ ContentId.fromString "Xar") (Config.dbConnPool config)
      let verificationToken = fromJust $ maybeContent >>= contentVerificationToken
      liftIO $ print verificationToken
      get ("/v1/content/Xar/verification/" <> (BC.pack $ show verificationToken))
        `shouldRespondWith` [r|{"dzi":null,"progress":0,"url":"http://example.com","verified":true,"embedHtml":"<script src=\"http://localhost:8000/Xar.js?width=auto&height=400px\"></script>","shareUrl":"http://localhost:8000/Xar","id":"Xar","ready":false,"failed":false}|]
          { matchStatus = 200,
            matchHeaders = [applicationJSON]
          }
    describe "Complete content by ID (PUT /v1/content/:id/completion)" do
      context "without auth" do
        it "should reject request" $
          putJSON
            "/v1/content/X75/completion"
            [r|{"type":"success","mime":"image/jpeg","size":1234,"dzi":{"width":456,"height":789,"tileSize":254,"tileOverlap":1,"tileFormat":"jpg"}}|]
            `shouldRespondWith` 401
      context "with invalid username" do
        it "should reject request" $
          authPutJSON
            "/v1/content/X75/completion"
            authorizedUser {username = "eve"}
            [r|{"type":"success","mime":"image/jpeg","size":1234,"dzi":{"width":456,"height":789,"tileSize":254,"tileOverlap":1,"tileFormat":"jpg"}}|]
            `shouldRespondWith` 401
      context "with invalid password" do
        it "should reject request" $
          authPutJSON
            "/v1/content/X75/completion"
            authorizedUser {password = "eve"}
            [r|{"type":"success","mime":"image/jpeg","size":1234,"dzi":{"width":456,"height":789,"tileSize":254,"tileOverlap":1,"tileFormat":"jpg"}}|]
            `shouldRespondWith` 401
      describe "content with 'initialized' state" do
        it "should accept success" $
          authPutJSON
            "/v1/content/X75/completion"
            authorizedUser
            [r|{"type":"success","mime":"image/jpeg","size":1234,"dzi":{"width":456,"height":789,"tileSize":254,"tileOverlap":1,"tileFormat":"jpg"}}|]
            `shouldRespondWith` [r|{"dzi":{"height":789,"url":"http://localhost:9000/_dzis_/X75.dzi","width":456,"tileOverlap":1,"tileFormat":"jpg","tileSize":254},"progress":1,"url":"http://e.i.uol.com.br/outros/0907/090731cielao1.jpg","verified":false,"embedHtml":"<script src=\"http://localhost:8000/X75.js?width=auto&height=400px\"></script>","shareUrl":"http://localhost:8000/X75","id":"X75","ready":true,"failed":false}|]
        it "should accept failure" $
          authPutJSON
            "/v1/content/yQ4/completion"
            authorizedUser
            [r|{"type": "failure", "error": "FAIL!"}|]
            `shouldRespondWith` [r|{"dzi":null,"progress":1,"url":"http://media.stenaline.com/media_SE/lalandia-map-zoomit/lalandia-map.jpg","verified":false,"embedHtml":"<script src=\"http://localhost:8000/yQ4.js?width=auto&height=400px\"></script>","shareUrl":"http://localhost:8000/yQ4","id":"yQ4","ready":false,"failed":true}|]
    describe "POST /v1/content?url=…" do
      it "should be rejected" $
        post "/v1/content?url=http://example.com" ""
          `shouldRespondWith` invalidHTTPMethod
    describe "PUT /v1/content?url=…" do
      it "should be rejected" $
        put "/v1/content?url=http://example.com" ""
          `shouldRespondWith` invalidHTTPMethod
  describe "JSONP" do
    describe "GET /v1/content?url=…&callback=…" do
      it "should accept `callback` query parameter" $
        get "/v1/content?callback=handleContent"
          `shouldRespondWith` [r|/**/ typeof handleContent === 'function' && handleContent({"status":400,"error":"Missing ID or URL. Please provide ID, e.g. `/v1/content/<id>`, or URL via `/v1/content?url=<url>` query parameter.","statusText":"Bad Request","redirectLocation":null});|]
            { matchStatus = 200,
              matchHeaders = [javaScriptUTF8]
            }
    describe "GET /v1/content/:id?callback=…" do
      it "should accept `callback` query parameter" do
        get "/v1/content/yQ4?callback=handleContent"
          `shouldRespondWith` [r|/**/ typeof handleContent === 'function' && handleContent({"status":200,"statusText":"OK","content":{"dzi":null,"progress":1,"url":"http://media.stenaline.com/media_SE/lalandia-map-zoomit/lalandia-map.jpg","verified":false,"embedHtml":"<script src=\"http://localhost:8000/yQ4.js?width=auto&height=400px\"></script>","shareUrl":"http://localhost:8000/yQ4","id":"yQ4","ready":false,"failed":true},"redirectLocation":null});|]
            { matchStatus = 200,
              matchHeaders = [javaScriptUTF8]
            }
  describe "View by URL (GET /:url)" do
    it "should return correct redirect existing content" do
      let (existingId, existingURL) = existingContent
       in get ("/" <> BC.pack existingURL)
            `shouldRespondWith` viewRedirect existingId
  describe "CORS" do
    it "should allow all origins" do
      let getWithHeader path headers = request methodGet path headers ""
       in getWithHeader "/v1/content/yQ4" [("Origin", "http://example.com")]
            `shouldRespondWith` 200
              { matchHeaders =
                  [ "Access-Control-Allow-Origin" <:> "*",
                    applicationJSON
                  ]
              }
  describe "Meta" do
    describe "Health (/health)" do
      it "should respond with `up`" do
        get "/health" `shouldRespondWith` "up" {matchStatus = 200}
    describe "Version (/version)" do
      it "should respond with version" do
        get "/version" `shouldRespondWith` "test" {matchStatus = 200}
  describe "Number of views" do
    context "when requesting content through REST API" do
      it "should increase `numViews`" do
        -- TODO: How can we avoid this dummy `Test.Hspec.Wai` request to satisfy
        -- type checker?
        get "/v1/content/yQ4" `shouldRespondWith` 200
        liftIO do
          let pool = Config.dbConnPool config
          maybeContent <- runPoolPQ (getById $ ContentId.fromString "yQ4") pool
          let numViews = maybe 0 contentNumViews maybeContent
          numViews `shouldBe` 5
  where
    authPutJSON path user = putJSON' path [(hAuthorization, toBasicAuthHeader user)]
    putJSON path = putJSON' path []
    putJSON' path headers =
      request
        methodPut
        path
        ( [ (hContentType, "application/json")
          ]
            <> headers
        )
    toBasicAuthHeader APIUser {username, password} =
      T.encodeUtf8 $ "Basic " <> T.encodeBase64 (username <> ":" <> password)
