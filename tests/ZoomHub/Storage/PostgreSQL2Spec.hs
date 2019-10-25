{-# OPTIONS_GHC -O0 #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Storage.PostgreSQL2Spec
  ( main
  , spec
  ) where

import Control.Exception (bracket)
import Control.Monad (void)
import qualified Data.ByteString.Char8 as BC
import Data.Function ((&))
import Data.Int (Int64)
import Data.Maybe (fromMaybe, maybe)
import qualified Data.Text as T
import Data.Time.Clock
  ( NominalDiffTime
  , UTCTime(..)
  , addUTCTime
  , diffTimeToPicoseconds
  , getCurrentTime
  , picosecondsToDiffTime
  )
import Data.Time.Units (Minute)
import qualified Generics.SOP as SOP
import Squeal.PostgreSQL
  ( Connection
  , Manipulation(UnsafeManipulation)
  , connectdb
  , finish
  , manipulate
  , pqThen
  , runPQ
  )
import Squeal.PostgreSQL.Migration (migrateDown, migrateUp)
import System.Environment (lookupEnv)
import System.Process (readProcess)
import Test.Hspec
  ( Spec
  , around
  , beforeAll_
  , context
  , describe
  , expectationFailure
  , hspec
  , it
  , shouldBe
  , shouldSatisfy
  )

import ZoomHub.Storage.PostgreSQL2
  ( dequeueNextUnprocessed
  , getById
  , getById'
  , getByURL
  , getByURL'
  , getExpiredActive
  , getNextUnprocessed
  , initialize
  , markAsActive
  , markAsFailure
  , markAsSuccess
  , resetAsInitialized
  )
import qualified ZoomHub.Storage.PostgreSQL2.Internal as I
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
import qualified ZoomHub.Types.Content.Internal as I
import qualified ZoomHub.Types.ContentId as ContentId
import qualified ZoomHub.Types.ContentMIME as ContentMIME
import ZoomHub.Types.ContentState (ContentState(..))
import ZoomHub.Types.ContentType (ContentType(..))
import ZoomHub.Types.ContentURI (ContentURI, ContentURI'(ContentURI))
import ZoomHub.Types.DeepZoomImage (mkDeepZoomImage)
import ZoomHub.Types.DeepZoomImage.TileFormat (TileFormat(JPEG))
import ZoomHub.Types.DeepZoomImage.TileOverlap (TileOverlap(TileOverlap1))
import ZoomHub.Types.DeepZoomImage.TileSize (TileSize(TileSize254))

main :: IO ()
main = hspec spec

defaultDatabaseUser :: String
defaultDatabaseUser = "zoomhub"

defaultDatabaseName :: String
defaultDatabaseName = "zoomhub_test"

setupDatabase :: IO ()
setupDatabase = do
  pgUser <- fromMaybe defaultDatabaseUser <$> lookupEnv "PGUSER"
  pgDatabase <- fromMaybe defaultDatabaseName <$> lookupEnv "PGDATABASE"

  void $ readProcess "dropdb" ["--username", pgUser, "--echo", "--if-exists", pgDatabase] []
  void $ readProcess "createdb" ["--owner", pgUser, pgDatabase] []

withDatabaseConnection :: (SOP.K Connection Schema -> IO a) -> IO a
withDatabaseConnection = bracket acquire release
  where
    acquire :: IO (SOP.K Connection Schema)
    acquire = do
      pgUser <- maybe (BC.pack defaultDatabaseUser) BC.pack <$> lookupEnv "PGUSER"
      pgDatabase <- maybe (BC.pack defaultDatabaseName) BC.pack <$> lookupEnv "PGDATABASE"

      conn <- connectdb $
        "host=localhost port=5432 dbname=" <> pgDatabase <> " user=" <> pgUser
      (_, conn') <- runPQ
        ( manipulate (UnsafeManipulation "SET client_min_messages TO WARNING;")
        & pqThen (migrateUp migrations)
        ) conn
      pure conn'
    release :: SOP.K Connection Schema -> IO ()
    release conn = do
      (_, conn') <- runPQ (migrateDown migrations) conn
      finish conn'

spec :: Spec
spec =
  beforeAll_ setupDatabase $ around withDatabaseConnection $ do
    describe "initialize" $
      it "should return initialized content" $ \conn -> do
        currentTime <- safeGetCurrentTime
        (mContent, _) <- runPQ (initialize testURL) conn
        case mContent of
          Just content -> do
            -- HACK: Hard-coded content ID set by database trigger
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

    describe "markAsActive" $
      it "should mark content as active" $ \conn -> do
        (mContent, _) <- runPQ (initialize testURL) conn
        case mContent of
          Just content -> do
            currentTime <- safeGetCurrentTime

            let cId = contentId content
            void $ runPQ (markAsActive cId) conn
            (result, _) <- runPQ (getById cId) conn

            case result >>= contentActiveAt of
              Just activeAt ->
                activeAt `shouldSatisfy` isWithinSecondsOf currentTime 3
              Nothing ->
                expectationFailure "expected `contentActiveAt` to be set"

            result `shouldBe` Just content
              { contentState = Active
              , contentType = Unknown
              , contentActiveAt = result >>= contentActiveAt
              , contentProgress = 0.0
              }
          Nothing ->
            expectationFailure "expected content to be initialized"

    describe "markAsSuccess" $
      it "should mark content as successful" $ \conn -> do
        (mContent, _) <- runPQ (initialize testURL) conn
        case mContent of
          Just content -> do
            currentTime <- safeGetCurrentTime

            let cId = contentId content
            let dzi = mkDeepZoomImage 300 400 TileSize254 TileOverlap1 JPEG
            let mMIME = ContentMIME.fromText "image/jpeg"
            let mSize = Just 1234
            void $ runPQ (markAsSuccess cId dzi mMIME mSize) conn
            (result, _) <- runPQ (getById cId) conn

            case result >>= contentCompletedAt of
              Just completedAt ->
                completedAt `shouldSatisfy` isWithinSecondsOf currentTime 3
              Nothing ->
                expectationFailure "expected `contentCompletedAt` to be set"

            result `shouldBe` Just content
              { contentState = CompletedSuccess
              , contentType = Image
              , contentDZI = Just dzi
              , contentCompletedAt = result >>= contentCompletedAt
              , contentProgress = 1.0
              , contentMIME = mMIME
              , contentSize = mSize
              }
          Nothing ->
            expectationFailure "expected content to be initialized"

    describe "markAsFailure" $
      it "should mark content as failure" $ \conn -> do
        (mContent, _) <- runPQ (initialize testURL) conn
        case mContent of
          Just content -> do
            currentTime <- safeGetCurrentTime

            let cId = contentId content
                errorMessage = Just "test error message"
            void $ runPQ (markAsFailure cId errorMessage) conn
            (result, _) <- runPQ (getById cId) conn

            case result >>= contentCompletedAt of
              Just t ->
                t `shouldSatisfy` isWithinSecondsOf currentTime 3
              Nothing ->
                expectationFailure "expected `contentCompletedAt` to be set"

            result `shouldBe` Just content
              { contentState = CompletedFailure
              , contentType = Unknown
              , contentCompletedAt = result >>= contentCompletedAt
              , contentError = errorMessage
              , contentProgress = 1.0
              }
          Nothing ->
            expectationFailure "expected content to be initialized"

    describe "resetAsInitialized" $
      it "should reset content as initialized" $ \conn -> do
        (mContent, _) <- runPQ (initialize testURL) conn
        case mContent of
          Just content -> do
            let cId = contentId content
                errorMessage = Just "test error message"
            void $ runPQ (markAsFailure cId errorMessage) conn
            void $ runPQ (resetAsInitialized cId) conn
            (result, _) <- runPQ (getById cId) conn

            result `shouldBe` Just content
          Nothing ->
            expectationFailure "expected content to be initialized"

    describe "dequeueNextUnprocessed" $ do
      context "when two content were initialized at the same time" $
        it "should return the one that is more popular (more views) and mark active" $ \conn -> do
          currentTime <- safeGetCurrentTime
          let minutes n = n * 60
              -- HACK: Hard-coded content IDs set by database trigger
              u1 = mkUnprocessedContent "X75" currentTime (15 & minutes) 100
              u2 = mkUnprocessedContent "yOJ" currentTime (15 & minutes) 200
              u3 = mkUnprocessedContent "yJL" currentTime (5 & minutes) 300
              u4 = mkActiveContent "foo" currentTime (45 & minutes)

          void $ runPQ (I.unsafeCreateContent u1) conn
          void $ runPQ (I.unsafeCreateContent u2) conn
          void $ runPQ (I.unsafeCreateContent u3) conn
          void $ runPQ (I.unsafeCreateContent u4) conn

          (result, _) <- runPQ dequeueNextUnprocessed conn

          case result >>= contentActiveAt of
            Just activeAt ->
              activeAt `shouldSatisfy` isWithinSecondsOf currentTime 3
            Nothing ->
              expectationFailure "expected `contentActiveAt` to be set"

          let dequeued = u2
                         { contentState = Active
                         , contentActiveAt = result >>= contentActiveAt
                         }
          result `shouldBe` Just dequeued

      context "when there is no initialized content" $
        it "should return nothing" $ \conn -> do
          currentTime <- safeGetCurrentTime

          let c1 = mkActiveContent "X75" currentTime 0
          void $ runPQ (I.unsafeCreateContent c1) conn

          (result, _) <- runPQ dequeueNextUnprocessed conn
          result `shouldBe` Nothing

      context "when there is no content" $
        it "should return nothing" $ \conn -> do
          (result, _) <- runPQ dequeueNextUnprocessed conn
          result `shouldBe` Nothing

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

    describe "getNextUnprocessed" $ do
      context "when two content were initialized at the same time" $
        it "should return the one that is more popular (more views)" $ \conn -> do
          currentTime <- safeGetCurrentTime
          let minutes n = n * 60
              -- HACK: Hard-coded content IDs set by database trigger
              u1 = mkUnprocessedContent "X75" currentTime (15 & minutes) 100
              u2 = mkUnprocessedContent "yOJ" currentTime (15 & minutes) 200
              u3 = mkUnprocessedContent "yJL" currentTime (5 & minutes) 300
              u4 = mkActiveContent "foo" currentTime (45 & minutes)

          void $ runPQ (I.unsafeCreateContent u1) conn
          void $ runPQ (I.unsafeCreateContent u2) conn
          void $ runPQ (I.unsafeCreateContent u3) conn
          void $ runPQ (I.unsafeCreateContent u4) conn

          (results, _) <- runPQ getNextUnprocessed conn
          results `shouldBe` Just u2

      context "when there is no initialized content" $
        it "should return nothing" $ \conn -> do
          currentTime <- safeGetCurrentTime

          let c1 = mkActiveContent "X75" currentTime 0
          void $ runPQ (I.unsafeCreateContent c1) conn

          (results, _) <- runPQ getNextUnprocessed conn
          results `shouldBe` Nothing

      context "when there is no content" $
        it "should return nothing" $ \conn -> do
          (results, _) <- runPQ getNextUnprocessed conn
          results `shouldBe` Nothing

    describe "getExpiredActive" $
      it "should return active content that has expired" $ \conn -> do
        currentTime <- safeGetCurrentTime
        let minutes n = n * 60
            -- HACK: Hard-coded content IDs set by database trigger
            c1 = mkActiveContent "X75" currentTime (15 & minutes)
            c2 = mkActiveContent "yOJ" currentTime (45 & minutes)
            c3 = mkActiveContent "yJL" currentTime (60 & minutes)

        void $ runPQ (I.unsafeCreateContent c1) conn
        void $ runPQ (I.unsafeCreateContent c2) conn
        void $ runPQ (I.unsafeCreateContent c3) conn

        (results, _) <- runPQ (getExpiredActive (30 :: Minute)) conn
        results `shouldBe` [c2, c3]

    where
    mkUnprocessedContent :: String -> UTCTime -> NominalDiffTime -> Int64 -> I.Content
    mkUnprocessedContent id_ currentTime age numViews =
      I.Content
        { contentId = ContentId.fromString id_
        , contentType = Unknown
        , contentURL = ContentURI $ "https://example.com/" <> T.pack id_
        , contentState = Initialized
        , contentInitializedAt = addUTCTime (-age) currentTime
        , contentActiveAt = Nothing
        , contentCompletedAt = Nothing
        , contentMIME = Nothing
        , contentSize = Nothing
        , contentProgress = 0.0
        , contentNumViews = numViews
        , contentError = Nothing
        , contentDZI = Nothing
        }

    mkActiveContent :: String -> UTCTime -> NominalDiffTime -> I.Content
    mkActiveContent id_ currentTime age =
      I.Content
        { contentId = ContentId.fromString id_
        , contentType = Image
        , contentURL = ContentURI $ "https://example.com/" <> T.pack id_
        , contentState = Active
        , contentInitializedAt = addUTCTime (-1) activeAt
        , contentActiveAt = Just activeAt
        , contentCompletedAt = Nothing
        , contentMIME = Nothing
        , contentSize = Nothing
        , contentProgress = 0.0
        , contentNumViews = 0
        , contentError = Nothing
        , contentDZI = Nothing
        }
      where
      activeAt = addUTCTime (-age) currentTime

    testURL :: ContentURI
    testURL = ContentURI "https://example.com/1"

    isWithinSecondsOf :: UTCTime -> NominalDiffTime -> UTCTime -> Bool
    isWithinSecondsOf pivot interval t =
      let upperBound = addUTCTime interval pivot
          lowerBound = addUTCTime (-interval) pivot
      in lowerBound <= t && t <= upperBound

    safeGetCurrentTime :: IO UTCTime
    safeGetCurrentTime = do
      ct@UTCTime{ utctDayTime } <- getCurrentTime
      let newDayTime = picosecondsToDiffTime $
                        (diffTimeToPicoseconds utctDayTime `div` 1000000) * 1000000
      return $ ct{ utctDayTime = newDayTime }
