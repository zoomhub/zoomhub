{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Storage.PostgreSQL2Spec
  ( main
  , spec
  ) where

import Control.Exception (bracket)
import qualified Generics.SOP as SOP
import Squeal.PostgreSQL (Connection, connectdb, define, finish, runPQ, (>>>))
import Test.Hspec
  (Spec, around, describe, expectationFailure, hspec, it, shouldBe)

import ZoomHub.Storage.PostgreSQL2 (getById, initialize)
import ZoomHub.Storage.PostgreSQL2.Schema (Schema, setup, teardown)
import ZoomHub.Types.Content (contentId)
import ZoomHub.Types.ContentURI (ContentURI'(ContentURI))

main :: IO ()
main = hspec spec

withDatabaseConnection :: (SOP.K Connection Schema -> IO a) -> IO a
withDatabaseConnection = bracket acquire release
  where
    acquire :: IO (SOP.K Connection Schema)
    acquire = do
      conn <- connectdb "host=localhost port=5432 dbname=zoomhub_test"
      (_, conn') <- runPQ (define (teardown >>> setup)) conn
      pure conn'
    release :: SOP.K Connection Schema -> IO ()
    release conn = do
      (_, conn') <- runPQ (define teardown) conn
      finish conn'

spec :: Spec
spec =
  around withDatabaseConnection $ do
    describe "getById" $
      it "should return item by hash ID" $ \conn -> do
        (mContent, _) <- runPQ (initialize $ ContentURI "http://example.com/1") conn
        case mContent of
          Just content -> do
            (result, _) <- runPQ (getById $ contentId content) conn
            result `shouldBe` (Just content)
          Nothing ->
            expectationFailure "expected content to be initialized"


