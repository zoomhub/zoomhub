{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -O0 #-}

module ZoomHub.Storage.PostgreSQLSpec
  ( main,
    spec,
  )
where

import Control.Exception (bracket)
import Control.Monad (forM_, void)
import qualified Data.ByteString.Char8 as BC
import Data.Function ((&))
import Data.Int (Int64)
import Data.Maybe (fromJust, isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
  ( NominalDiffTime,
    UTCTime (..),
    addUTCTime,
    diffTimeToPicoseconds,
    getCurrentTime,
    picosecondsToDiffTime,
  )
import Data.Time.Units (Minute)
import qualified Generics.SOP as SOP
import Squeal.PostgreSQL
  ( Connection,
    Manipulation (UnsafeManipulation),
    connectdb,
    finish,
    manipulate_,
    pqThen,
    runPQ,
  )
import Squeal.PostgreSQL.Session.Migration (migrateDown, migrateUp)
import System.Environment (lookupEnv)
import System.Process (readProcess)
import Test.Hspec
  ( Spec,
    around,
    beforeAll_,
    context,
    describe,
    expectationFailure,
    hspec,
    it,
    shouldBe,
    shouldSatisfy,
  )
import ZoomHub.Storage.PostgreSQL
  ( dequeueNextUnprocessed,
    getById,
    getById',
    getByURL,
    getByURL',
    getExpiredActive,
    getNextUnprocessed,
    initialize,
    markAsActive,
    markAsFailure,
    markAsSuccess,
    resetAsInitialized,
    unsafeResetAsInitializedWithVerification,
  )
import qualified ZoomHub.Storage.PostgreSQL.ConnectInfo as ConnectInfo
import qualified ZoomHub.Storage.PostgreSQL.Internal as I
import ZoomHub.Storage.PostgreSQL.Schema (Schemas, migrations)
import ZoomHub.Types.Content
  ( contentActiveAt,
    contentCompletedAt,
    contentDZI,
    contentError,
    contentId,
    contentInitializedAt,
    contentMIME,
    contentNumViews,
    contentProgress,
    contentSize,
    contentState,
    contentSubmitterEmail,
    contentType,
    contentURL,
    contentUserId,
    contentVerificationToken,
    contentVerifiedAt,
  )
import qualified ZoomHub.Types.Content.Internal as I
import ZoomHub.Types.ContentId (ContentId)
import qualified ZoomHub.Types.ContentId as ContentId
import qualified ZoomHub.Types.ContentMIME as ContentMIME
import ZoomHub.Types.ContentState (ContentState (..))
import ZoomHub.Types.ContentType (ContentType (..))
import ZoomHub.Types.ContentURI (ContentURI (ContentURI))
import ZoomHub.Types.DeepZoomImage (mkDeepZoomImage)
import ZoomHub.Types.DeepZoomImage.TileFormat (TileFormat (JPEG))
import ZoomHub.Types.DeepZoomImage.TileOverlap (TileOverlap (TileOverlap1))
import ZoomHub.Types.DeepZoomImage.TileSize (TileSize (TileSize254))
import ZoomHub.Types.VerificationToken (VerificationToken)
import qualified ZoomHub.Types.VerificationToken as VerificationToken

main :: IO ()
main = hspec spec

defaultDatabaseUser :: String
defaultDatabaseUser = "zoomhub"

defaultDatabaseName :: String
defaultDatabaseName = "zoomhub_test"

hashidsSecret :: String
hashidsSecret = "secret-salt"

unsafeContentId :: String -> ContentId
unsafeContentId id_ =
  case ContentId.fromString id_ of
    Just cId -> cId
    Nothing -> error $ "Invalid content ID: " <> id_

setupDatabase :: IO ()
setupDatabase = do
  connectInfo <- ConnectInfo.fromEnv defaultDatabaseName
  let pgHost = ConnectInfo.connectHost connectInfo
      pgPort = ConnectInfo.connectPort connectInfo
      pgUser = ConnectInfo.connectUser connectInfo
      pgDatabase = ConnectInfo.connectDatabase connectInfo
  void $
    readProcess
      "dropdb"
      [ "--host",
        pgHost,
        "--port",
        show pgPort,
        "--username",
        pgUser,
        "--echo",
        "--if-exists",
        pgDatabase
      ]
      []
  void $
    readProcess
      "createdb"
      [ "--host",
        pgHost,
        "--port",
        show pgPort,
        "--username",
        pgUser,
        "--echo",
        "--owner",
        pgUser,
        pgDatabase
      ]
      []

withDatabaseConnection :: (SOP.K Connection Schemas -> IO a) -> IO a
withDatabaseConnection = bracket acquire release
  where
    acquire :: IO (SOP.K Connection Schemas)
    acquire = do
      pgUser <- maybe (BC.pack defaultDatabaseUser) BC.pack <$> lookupEnv "PGUSER"
      pgDatabase <- maybe (BC.pack defaultDatabaseName) BC.pack <$> lookupEnv "PGDATABASE"
      conn <-
        connectdb $
          "host=localhost port=5432 dbname=" <> pgDatabase <> " user=" <> pgUser
      (_, conn') <-
        runPQ
          ( manipulate_ (UnsafeManipulation "SET client_min_messages TO WARNING;")
              & pqThen (migrateUp (migrations hashidsSecret))
          )
          conn
      pure conn'

    release :: SOP.K Connection Schemas -> IO ()
    release conn = do
      (_, conn') <- runPQ (migrateDown (migrations hashidsSecret)) conn
      finish conn'

spec :: Spec
spec =
  beforeAll_ setupDatabase $ around withDatabaseConnection do
    describe "initialize" do
      it "should return initialized content" do
        \conn -> do
          currentTime <- safeGetCurrentTime
          (mContent, _) <- runPQ (initialize testURL testEmail) conn
          case mContent of
            Just content -> do
              -- HACK: Hard-coded content ID set by database trigger
              contentId content `shouldBe` unsafeContentId "X75"
              contentType content `shouldBe` Unknown
              contentURL content `shouldBe` testURL
              contentState content `shouldBe` Initialized
              contentInitializedAt content
                `shouldSatisfy` isWithinSecondsOf currentTime 3
              contentActiveAt content `shouldBe` Nothing
              contentCompletedAt content `shouldBe` Nothing
              contentMIME content `shouldBe` Nothing
              contentSize content `shouldBe` Nothing
              contentProgress content `shouldBe` 0.0
              contentNumViews content `shouldBe` 0
              contentError content `shouldBe` Nothing
              contentDZI content `shouldBe` Nothing
              contentSubmitterEmail content `shouldBe` Just testEmail
            Nothing ->
              expectationFailure "expected content to be initialized"

    describe "markAsActive" do
      it "should mark content as active" do
        \conn -> do
          (mContent, _) <- runPQ (initialize testURL testEmail) conn
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
              result
                `shouldBe` Just
                  content
                    { contentState = Active,
                      contentType = Unknown,
                      contentActiveAt = result >>= contentActiveAt,
                      contentProgress = 0.0
                    }
            Nothing ->
              expectationFailure "expected content to be initialized"
    describe "markAsSuccess" do
      it "should mark content as successful" do
        \conn -> do
          (mContent, _) <- runPQ (initialize testURL testEmail) conn
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
              result
                `shouldBe` Just
                  content
                    { contentState = CompletedSuccess,
                      contentType = Image,
                      contentDZI = Just dzi,
                      contentCompletedAt = result >>= contentCompletedAt,
                      contentProgress = 1.0,
                      contentMIME = mMIME,
                      contentSize = mSize
                    }
            Nothing ->
              expectationFailure "expected content to be initialized"
    describe "markAsFailure" do
      it "should mark content as failure" do
        \conn -> do
          (mContent, _) <- runPQ (initialize testURL testEmail) conn
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
              result
                `shouldBe` Just
                  content
                    { contentState = CompletedFailure,
                      contentType = Unknown,
                      contentCompletedAt = result >>= contentCompletedAt,
                      contentError = errorMessage,
                      contentProgress = 1.0
                    }
            Nothing ->
              expectationFailure "expected content to be initialized"
    describe "resetAsInitialized" do
      it "should reset content as initialized" do
        \conn -> do
          (mContent, _) <- runPQ (initialize testURL testEmail) conn
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
    describe "unsafeResetAsInitializedWithVerification" do
      it "should reset content as initialized and verify it" do
        \conn -> do
          (mContent, _) <- runPQ (initialize testURL testEmail) conn
          case mContent of
            Just content -> do
              let cId = contentId content
                  errorMessage = Just "test error message"
              void $ runPQ (markAsFailure cId errorMessage) conn
              void $ runPQ (unsafeResetAsInitializedWithVerification cId) conn
              (Just result, _) <- runPQ (getById cId) conn
              contentVerifiedAt result `shouldSatisfy` isJust
              result {contentVerifiedAt = Nothing} `shouldBe` content
            Nothing ->
              expectationFailure "expected content to be initialized"
    describe "dequeueNextUnprocessed" do
      context "when two content were initialized at the same time" do
        it "should return the one that is more popular (more views) and mark active" do
          \conn -> do
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
            let dequeued =
                  u2
                    { contentState = Active,
                      contentActiveAt = result >>= contentActiveAt
                    }
            result `shouldBe` Just dequeued
      context "when there is no initialized content" do
        it "should return nothing" do
          \conn -> do
            currentTime <- safeGetCurrentTime
            let c1 = mkActiveContent "X75" currentTime 0
            void $ runPQ (I.unsafeCreateContent c1) conn
            (result, _) <- runPQ dequeueNextUnprocessed conn
            result `shouldBe` Nothing
      context "when there is no content" do
        it "should return nothing" do
          \conn -> do
            (result, _) <- runPQ dequeueNextUnprocessed conn
            result `shouldBe` Nothing
    describe "getById" do
      it "should return item by hash ID" do
        \conn -> do
          (mContent, _) <- runPQ (initialize testURL testEmail) conn
          case mContent of
            Just content -> do
              (result, _) <- runPQ (getById $ contentId content) conn
              result `shouldBe` Just content
            Nothing ->
              expectationFailure "expected content to be initialized"
    describe "getById'" do
      it "should increase number of views" do
        \conn -> do
          (mContent, _) <- runPQ (initialize testURL testEmail) conn
          case mContent of
            Just content -> do
              let cId = contentId content
              void $ runPQ (getById' cId) conn
              void $ runPQ (getById' cId) conn
              void $ runPQ (getById' cId) conn
              (result, _) <- runPQ (getById cId) conn
              result `shouldBe` Just (content {contentNumViews = 3})
            Nothing ->
              expectationFailure "expected content to be initialized"
    describe "getByURL" do
      it "should return item by URL" do
        \conn -> do
          (mContent, _) <- runPQ (initialize testURL testEmail) conn
          case mContent of
            Just content -> do
              (result, _) <- runPQ (getByURL testURL) conn
              result `shouldBe` Just content
            Nothing ->
              expectationFailure "expected content to be initialized"
    describe "getByURL'" do
      it "should increase number of views" do
        \conn -> do
          (mContent, _) <- runPQ (initialize testURL testEmail) conn
          case mContent of
            Just content -> do
              let url = contentURL content
              void $ runPQ (getByURL' url) conn
              void $ runPQ (getByURL' url) conn
              void $ runPQ (getByURL' url) conn
              (result, _) <- runPQ (getByURL url) conn
              result `shouldBe` Just (content {contentNumViews = 3})
            Nothing ->
              expectationFailure "expected content to be initialized"
    describe "getNextUnprocessed" do
      context "when two content were initialized at the same time" do
        it "should return the one that is more popular (more views)" do
          \conn -> do
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
      context "when there is no initialized content" do
        it "should return nothing" do
          \conn -> do
            currentTime <- safeGetCurrentTime
            let c1 = mkActiveContent "X75" currentTime 0
            void $ runPQ (I.unsafeCreateContent c1) conn
            (results, _) <- runPQ getNextUnprocessed conn
            results `shouldBe` Nothing
      context "when there is no content" do
        it "should return nothing" do
          \conn -> do
            (results, _) <- runPQ getNextUnprocessed conn
            results `shouldBe` Nothing
    describe "getExpiredActive" do
      it "should return active content that has expired" do
        \conn -> do
          currentTime <- safeGetCurrentTime
          let minutes n = n * 60
              -- HACK: Hard-coded content IDs set by database trigger
              c1 = mkActiveContent "X75" currentTime (15 & minutes)
              c2 = mkActiveContent "yOJ" currentTime (45 & minutes)
              c3 = mkActiveContent "yJL" currentTime (60 & minutes)
              c4 = mkSucceededContent "foo" currentTime (90 & minutes)
          void . forM_ [c1, c2, c3, c4] $ \c ->
            runPQ (I.unsafeCreateContent c) conn
          (results, _) <- runPQ (getExpiredActive (30 :: Minute)) conn
          results `shouldBe` [c2, c3]
  where
    mkUnprocessedContent :: String -> UTCTime -> NominalDiffTime -> Int64 -> I.Content
    mkUnprocessedContent id_ currentTime age numViews =
      I.Content
        { contentId = unsafeContentId id_,
          contentType = Unknown,
          contentURL = ContentURI $ "https://example.com/" <> T.pack id_,
          contentState = Initialized,
          contentInitializedAt = initializedAt,
          contentActiveAt = Nothing,
          contentCompletedAt = Nothing,
          contentMIME = Nothing,
          contentSize = Nothing,
          contentProgress = 0.0,
          contentNumViews = numViews,
          contentError = Nothing,
          contentDZI = Nothing,
          contentSubmitterEmail = Nothing,
          contentVerificationToken = Just nullVerificationToken,
          contentVerifiedAt = Just initializedAt,
          contentUserId = Nothing
        }
      where
        initializedAt = addUTCTime (-age) currentTime
    mkActiveContent :: String -> UTCTime -> NominalDiffTime -> I.Content
    mkActiveContent id_ currentTime age =
      I.Content
        { contentId = unsafeContentId id_,
          contentType = Unknown,
          contentURL = ContentURI $ "https://example.com/" <> T.pack id_,
          contentState = Active,
          contentInitializedAt = addUTCTime (-1) activeAt,
          contentActiveAt = Just activeAt,
          contentCompletedAt = Nothing,
          contentMIME = Nothing,
          contentSize = Nothing,
          contentProgress = 0.0,
          contentNumViews = 0,
          contentError = Nothing,
          contentDZI = Nothing,
          contentSubmitterEmail = Nothing,
          contentVerificationToken = Just nullVerificationToken,
          contentVerifiedAt = Just initializedAt,
          contentUserId = Nothing
        }
      where
        initializedAt = addUTCTime (-1) activeAt
        activeAt = addUTCTime (-age) currentTime
    mkSucceededContent :: String -> UTCTime -> NominalDiffTime -> I.Content
    mkSucceededContent id_ currentTime age =
      let dzi = mkDeepZoomImage 300 400 TileSize254 TileOverlap1 JPEG
          mMIME = ContentMIME.fromText "image/jpeg"
          mSize = Just 1234
       in I.Content
            { contentId = unsafeContentId id_,
              contentType = Image,
              contentURL = ContentURI $ "https://example.com/" <> T.pack id_,
              contentState = CompletedSuccess,
              contentInitializedAt = initializedAt,
              contentActiveAt = Just activeAt,
              contentCompletedAt = Just (addUTCTime 1 activeAt),
              contentMIME = mMIME,
              contentSize = mSize,
              contentProgress = 1.0,
              contentNumViews = 0,
              contentError = Nothing,
              contentDZI = Just dzi,
              contentSubmitterEmail = Nothing,
              contentVerificationToken = Just nullVerificationToken,
              contentVerifiedAt = Just initializedAt,
              contentUserId = Nothing
            }
      where
        activeAt = addUTCTime (-age) currentTime
        initializedAt = addUTCTime (-1) activeAt

    testURL :: ContentURI
    testURL = ContentURI "https://example.com/1"

    testEmail :: Text
    testEmail = "test@example.com"

    isWithinSecondsOf :: UTCTime -> NominalDiffTime -> UTCTime -> Bool
    isWithinSecondsOf pivot interval t =
      let upperBound = addUTCTime interval pivot
          lowerBound = addUTCTime (-interval) pivot
       in lowerBound <= t && t <= upperBound

    safeGetCurrentTime :: IO UTCTime
    safeGetCurrentTime = do
      ct@UTCTime {utctDayTime} <- getCurrentTime
      let newDayTime =
            picosecondsToDiffTime $
              (diffTimeToPicoseconds utctDayTime `div` 1000000) * 1000000
      return $ ct {utctDayTime = newDayTime}

    nullVerificationToken :: VerificationToken
    nullVerificationToken =
      fromJust $ VerificationToken.fromText "00000000-0000-0000-0000-000000000000"
