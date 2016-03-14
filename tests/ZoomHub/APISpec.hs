{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.APISpec
  ( main
  , spec
  ) where

import           Data.Maybe                           (fromJust)
import           Network.URI                          (URI, parseAbsoluteURI)
import           Network.Wai                          (Middleware)
import           Test.Hspec                           (Spec, context, describe,
                                                       hspec, it)
import           Test.Hspec.Wai                       (get, shouldRespondWith,
                                                       with)

import           ZoomHub.API                          (app)
import           ZoomHub.Config                       (Config (..))
import           ZoomHub.Types.BaseURI                (BaseURI (BaseURI))
import           ZoomHub.Types.ContentBaseURI         (ContentBaseURI (ContentBaseURI))



main :: IO ()
main = hspec spec

toURI :: String -> URI
toURI = fromJust . parseAbsoluteURI

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
  describe "GET /v1/content" $ do
    context "without `id` or `url` query parameter" $ do
      it "responds with 400" $ do
        get "/v1/content" `shouldRespondWith` 400
