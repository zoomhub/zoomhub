{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Storage.PostgreSQL2Spec
  ( main
  , spec
  ) where

import Control.Exception (bracket)
import Control.Monad (void)
import qualified Generics.SOP as SOP
import Squeal.PostgreSQL (Connection, connectdb, finish, runPQ)
import Squeal.PostgreSQL.Migration (migrateDown, migrateUp)
import Test.Hspec
  ( Spec
  , around
  , describe
  , expectationFailure
  , hspec
  , it
  , shouldBe
  , shouldSatisfy
  )

import Data.Time.Clock (NominalDiffTime, UTCTime, addUTCTime, getCurrentTime)
import ZoomHub.Storage.PostgreSQL2
  (getById, getById', getByURL, getByURL', initialize)
import ZoomHub.Storage.PostgreSQL2.Schema (Schema, migrations)
import ZoomHub.Types.Content
  ( contentActiveAt
  , contentCompletedAt
  , contentDZI
  , contentError
  , contentId
  , contentInitializedAt
  , contentMIME
  , contentNumViews
  , contentProgress
  , contentSize
  , contentState
  , contentType
  , contentURL
  )
import qualified ZoomHub.Types.ContentId as ContentId
import ZoomHub.Types.ContentState (ContentState(Initialized))
import ZoomHub.Types.ContentType (ContentType(Unknown))
import ZoomHub.Types.ContentURI (ContentURI, ContentURI'(ContentURI))

main :: IO ()
main = hspec spec

withDatabaseConnection :: (SOP.K Connection Schema -> IO a) -> IO a
withDatabaseConnection = bracket acquire release
  where
    acquire :: IO (SOP.K Connection Schema)
    acquire = do
      conn <- connectdb "host=localhost port=5432 dbname=zoomhub_test"
      (_, conn') <- runPQ (migrateUp migrations) conn
      pure conn'
    release :: SOP.K Connection Schema -> IO ()
    release conn = do
      (_, conn') <- runPQ (migrateDown migrations) conn
      finish conn'

spec :: Spec
spec =
  around withDatabaseConnection $ do
    describe "initialize" $
      it "should return initialized content" $ \conn -> do
        currentTime <- getCurrentTime
        (mContent, _) <- runPQ (initialize testURL) conn
        case mContent of
          Just content -> do
            contentId content `shouldBe` ContentId.fromString "X75"
            contentType content `shouldBe` Unknown
            contentURL content `shouldBe` testURL
            contentState content `shouldBe` Initialized
            contentInitializedAt content `shouldSatisfy`
              isWithinSecondsOf currentTime 3
            contentActiveAt content `shouldBe` Nothing
            contentCompletedAt content `shouldBe` Nothing
            contentMIME content `shouldBe` Nothing
            contentSize content `shouldBe` Nothing
            contentProgress content `shouldBe` 0.0
            contentNumViews content `shouldBe` 0
            contentError content `shouldBe` Nothing
            contentDZI content `shouldBe` Nothing
          Nothing ->
            expectationFailure "expected content to be initialized"

    describe "getById" $
      it "should return item by hash ID" $ \conn -> do
        (mContent, _) <- runPQ (initialize testURL) conn
        case mContent of
          Just content -> do
            (result, _) <- runPQ (getById $ contentId content) conn
            result `shouldBe` Just content
          Nothing ->
            expectationFailure "expected content to be initialized"

    describe "getById'" $
      it "should increase number of views" $ \conn -> do
        (mContent, _) <- runPQ (initialize testURL) conn
        case mContent of
          Just content -> do
            let cId = contentId content
            void $ runPQ (getById' cId) conn
            void $ runPQ (getById' cId) conn
            void $ runPQ (getById' cId) conn
            (result, _) <- runPQ (getById cId) conn
            result `shouldBe` Just (content{ contentNumViews = 3 })
          Nothing ->
            expectationFailure "expected content to be initialized"

    describe "getByURL" $
      it "should return item by URL" $ \conn -> do
        (mContent, _) <- runPQ (initialize testURL) conn
        case mContent of
          Just content -> do
            (result, _) <- runPQ (getByURL testURL) conn
            result `shouldBe` Just content
          Nothing ->
            expectationFailure "expected content to be initialized"

    describe "getByURL'" $
      it "should increase number of views" $ \conn -> do
        (mContent, _) <- runPQ (initialize testURL) conn
        case mContent of
          Just content -> do
            let url = contentURL content
            void $ runPQ (getByURL' url) conn
            void $ runPQ (getByURL' url) conn
            void $ runPQ (getByURL' url) conn
            (result, _) <- runPQ (getByURL url) conn
            result `shouldBe` Just (content{ contentNumViews = 3 })
          Nothing ->
            expectationFailure "expected content to be initialized"

    where
    testURL :: ContentURI
    testURL = ContentURI "https://example.com/1"

    isWithinSecondsOf :: UTCTime -> NominalDiffTime -> UTCTime -> Bool
    isWithinSecondsOf pivot interval t =
      let upperBound = addUTCTime interval pivot
          lowerBound = addUTCTime (-interval) pivot
      in lowerBound <= t && t <= upperBound
