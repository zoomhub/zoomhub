{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.APISpec
  ( main
  , spec
  ) where

import qualified Data.ByteString.Char8            as BC
import           Data.Maybe                       (fromJust)
import           Data.Monoid                      ((<>))
import           Network.URI                      (URI, parseAbsoluteURI)
import           Network.Wai                      (Middleware)
import           Test.Hspec                       (Spec, describe, hspec, it)
import           Test.Hspec.Wai                   (MatchHeader, ResponseMatcher,
                                                   get, matchHeaders,
                                                   matchStatus, post, put,
                                                   shouldRespondWith, with,
                                                   (<:>))

import           ZoomHub.API                      (app)
import           ZoomHub.Config                   (Config (..))
import qualified ZoomHub.Config                   as Config
import           ZoomHub.Types.BaseURI            (BaseURI (BaseURI))
import           ZoomHub.Types.ContentBaseURI     (ContentBaseURI (ContentBaseURI))
import           ZoomHub.Types.Internal.ContentId (ContentId, fromString, unId)

main :: IO ()
main = hspec spec

-- Helpers
toURI :: String -> URI
toURI = fromJust . parseAbsoluteURI

existingContent :: (ContentId, String)
existingContent = (fromString "h",
  "http://upload.wikimedia.org/wikipedia/commons/3/36/SeattleI5Skyline.jpg")

-- Matchers
plainTextUTF8 :: MatchHeader
plainTextUTF8 = "Content-Type" <:> "text/plain; charset=utf-8"

plainText :: MatchHeader
plainText = "Content-Type" <:> "text/plain"

javaScriptUTF8 :: MatchHeader
javaScriptUTF8 = "Content-Type" <:> "application/javascript;charset=utf-8"

invalidURL :: ResponseMatcher
invalidURL =
  "Please give us the full URL, including ‘http://’ or ‘https://’."
  { matchStatus = 400
  , matchHeaders = [plainTextUTF8]
  }

invalidHTTPMethod :: ResponseMatcher
invalidHTTPMethod =
  "Only GET or HEAD is supported"
  { matchStatus = 405
  , matchHeaders = [plainText]
  }

noNewContent :: ResponseMatcher
noNewContent =
  "We are currently not processing new content."
  { matchStatus = 503
  , matchHeaders = [plainTextUTF8]
  }

restRedirect :: ContentId -> ResponseMatcher
restRedirect cId =
    ""
    { matchStatus = 301
    , matchHeaders = ["Location" <:> BC.pack expectedLocation]
    }
  where
    baseURIPrefix = show . Config.baseURI $ config
    expectedLocation = baseURIPrefix ++ "/v1/content/" ++ unId cId

-- Config
nullLogger :: Middleware
nullLogger = id

config :: Config
config = Config
  { acceptNewContent = False
  , baseURI = BaseURI $ toURI "http://localhost:8000"
  , contentBaseURI = ContentBaseURI $ toURI "http://localhost:9000"
  , dataPath = "./data"
  , encodeId = show
  , error404 = "404"
  , jobs = undefined
  , lastId = undefined
  , logger = nullLogger
  , openseadragonScript = "osd"
  , port = 8000
  , publicPath = "./public"
  , rackspace = undefined
  , version = "test"
  }

spec :: Spec
spec = with (return $ app config) $ do
  describe "Health" $
    it "respond with `up` and status 200" $
      get "/health" `shouldRespondWith` "up" {matchStatus = 200}

  describe "RESTful" $ do
    describe "List (GET /v1/content)" $
        it "should be interpreted as a ‘get by URL’, with no URL given" $
          get "/v1/content" `shouldRespondWith` "Missing ID or URL.\
            \ Please provide ID, e.g. `/v1/content/<id>`,\
            \ or URL via `/v1/content?url=<url>` query parameter."
            { matchStatus = 400
            , matchHeaders = [plainTextUTF8]
            }

    describe "Get by URL (GET /v1/content?url=…)" $ do
      it "should reject empty URLs" $
        get "/v1/content?url=" `shouldRespondWith`
          invalidURL

      it "should reject malformed URLs" $
        get "/v1/content?url=lasjdoasj)(¨‚Ô‚ˆÔ∏ŒÂ;sd)" `shouldRespondWith`
          invalidURL

      it "should reject URLs without protocol" $
        get "/v1/content?url=example.com" `shouldRespondWith`
          invalidURL

      it "should reject URLs with non-HTTP protocol" $ do
        get "/v1/content?url=ftp://example.com" `shouldRespondWith`
          invalidURL

        get "/v1/content?url=mailto://example@example.com" `shouldRespondWith`
          invalidURL

      it "should reject new HTTP URLs (for now)" $
        get "/v1/content?url=http://example.com" `shouldRespondWith`
          noNewContent

      it "should redirect existing (converted) HTTP URLs to ID" $
        let (existingId, existingURL) = existingContent in
        get ("/v1/content?url=" <> BC.pack existingURL) `shouldRespondWith`
          restRedirect existingId

    describe "POST /v1/content?url=…" $
      it "should be rejected" $
        post "/v1/content?url=http://example.com" "" `shouldRespondWith`
          invalidHTTPMethod

    describe "PUT /v1/content?url=…" $
      it "should be rejected" $
        put "/v1/content?url=http://example.com" "" `shouldRespondWith`
          invalidHTTPMethod

  describe "JSONP" $ do
    describe "GET /v1/content?url=…&callback=…" $
      it "should accept `callback` query parameter" $
        get "/v1/content?callback=handleContent" `shouldRespondWith`
          "/**/ typeof handleContent === 'function' &&\
          \ handleContent({\"status\":400,\"error\":\"Missing ID or URL.\
          \ Please provide ID, e.g. `/v1/content/<id>`, or URL via\
          \ `/v1/content?url=<url>` query parameter.\",\"statusText\":\
          \\"Bad Request\",\"redirectLocation\":null});"
          { matchStatus = 200
          , matchHeaders = [javaScriptUTF8]
          }

    describe "GET /v1/content?url=…&callback=…" $
      it "should accept `callback` query parameter" $
        get "/v1/content?callback=handleContent" `shouldRespondWith`
          "/**/ typeof handleContent === 'function' &&\
          \ handleContent({\"status\":400,\"error\":\"Missing ID or URL.\
          \ Please provide ID, e.g. `/v1/content/<id>`, or URL via\
          \ `/v1/content?url=<url>` query parameter.\",\"statusText\":\
          \\"Bad Request\",\"redirectLocation\":null});"
          { matchStatus = 200
          , matchHeaders = [javaScriptUTF8]
          }
