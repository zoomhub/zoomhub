{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.APISpec
  ( main
  , spec
  ) where

import           Data.Maybe                   (fromJust)
import           Network.URI                  (URI, parseAbsoluteURI)
import           Network.Wai                  (Middleware)
import           Test.Hspec                   (Spec, context, describe, hspec,
                                               it)
import           Test.Hspec.Wai               (MatchHeader, get, matchHeaders,
                                               matchStatus, post, put,
                                               shouldRespondWith, with, (<:>))

import           ZoomHub.API                  (app)
import           ZoomHub.Config               (Config (..))
import           ZoomHub.Types.BaseURI        (BaseURI (BaseURI))
import           ZoomHub.Types.ContentBaseURI (ContentBaseURI (ContentBaseURI))



main :: IO ()
main = hspec spec

-- Helpers
toURI :: String -> URI
toURI = fromJust . parseAbsoluteURI

-- Matchers
plainTextUTF8 :: MatchHeader
plainTextUTF8 = "Content-Type" <:> "text/plain; charset=utf-8"

plainText :: MatchHeader
plainText = "Content-Type" <:> "text/plain"

-- Config
nullLogger :: Middleware
nullLogger = id

config :: Config
config = Config
  { acceptNewContent = False
  , baseURI = BaseURI $ toURI "http://localhost:9999"
  , contentBaseURI = ContentBaseURI $ toURI "http://localhost:9998"
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
  -- Health
  describe "GET /v1/health" $
    it "respond with `up` and status 200" $
      get "/health" `shouldRespondWith` "up" {matchStatus = 200}

  -- RESTful
  describe "GET /v1/content" $
    context "without `id` segment or `url` query parameter" $
      it "responds with 400" $
        get "/v1/content" `shouldRespondWith` "Missing ID or URL.\
          \ Please provide ID, e.g. `/v1/content/<id>`,\
          \ or URL via `/v1/content?url=<url>` query parameter."
          { matchStatus = 400
          , matchHeaders = [plainTextUTF8]
          }

  describe "GET /v1/content?url=example.com" $
    context "with invalid `url` query parameter" $
      it "responds with 400" $
        get "/v1/content?url=example.com" `shouldRespondWith`
          "Please give us the full URL, including ‘http://’ or ‘https://’."
          { matchStatus = 400
          , matchHeaders = [plainTextUTF8]
          }

  describe "GET /v1/content?url=http://example.com" $
    context "with invalid `url` query parameter" $
      it "responds with 503" $
        get "/v1/content?url=http://example.com" `shouldRespondWith`
          "We are currently not processing new content."
          { matchStatus = 503
          , matchHeaders = [plainTextUTF8]
          }

  describe "POST /v1/content?url=http://example.com" $
    context "with invalid POST method" $
      it "responds with 503" $
        post "/v1/content?url=http://example.com" "" `shouldRespondWith`
          "Only GET or HEAD is supported"
          { matchStatus = 405
          , matchHeaders = [plainText]
          }

  describe "PUT /v1/content?url=http://example.com" $
    context "with invalid PUT method" $
      it "responds with 405" $
        put "/v1/content?url=http://example.com" "" `shouldRespondWith`
          "Only GET or HEAD is supported"
          { matchStatus = 405
          , matchHeaders = [plainText]
          }
