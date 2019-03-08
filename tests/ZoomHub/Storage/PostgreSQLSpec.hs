{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Storage.PostgreSQLSpec
  ( main
  , spec
  ) where

import           Control.Exception                   (bracket)
import           Data.Time.Clock                     (UTCTime, getCurrentTime)
import           Data.Time.Units                     (Minute)
import qualified Database.PostgreSQL.Simple          as PGS
import           Test.Hspec                          (Spec, around, describe,
                                                      hspec, it, shouldReturn)

import           ZoomHub.Storage.PostgreSQL          (ConnectInfo (..),
                                                      defaultConnectInfo,
                                                      getExpiredActive,
                                                      getNextUnprocessed,
                                                      runInsertContent)
import           ZoomHub.Storage.PostgreSQL.Internal (subtractUTCTime,
                                                      toNominalDiffTime)
import           ZoomHub.Types.Content               (contentActiveAt,
                                                      contentNumViews,
                                                      contentState, mkContent)
import qualified ZoomHub.Types.ContentId             as ContentId
import qualified ZoomHub.Types.ContentState          as ContentState
import           ZoomHub.Types.ContentType           (ContentType (Image))
import           ZoomHub.Types.ContentURI            (ContentURI' (ContentURI))


main :: IO ()
main = hspec spec

openConnection :: IO PGS.Connection
openConnection = PGS.connect connInfo
  where
    connInfo = defaultConnectInfo
                { connectDatabase = "zoomhub_test"
                }

closeConnection :: PGS.Connection -> IO ()
closeConnection = PGS.close

withDatabaseConnection :: (PGS.Connection -> IO ()) -> IO ()
withDatabaseConnection = bracket openConnection closeConnection

spec :: Spec
spec = return ()
  -- around withDatabaseConnection $ do
  --   describe "getNextUnprocessed" $ do
  --     it "should return most viewed item that hasn’t been converted" $ \conn -> do
  --       let initializedContent = mkContent Image
  --             (ContentId.fromString "6")
  --             (ContentURI "http://example.com/6")
  --             (read "2017-01-01 00:00:00Z" :: UTCTime)
  --           initializedContentWithViews = initializedContent
  --             { contentNumViews = 100
  --             }

  --           completedContent = mkContent Image
  --             (ContentId.fromString "7")
  --             (ContentURI "http://example.com/7")
  --             (read "2016-01-01 00:00:00Z" :: UTCTime)
  --           completedContentWithViews = completedContent
  --             { contentState = ContentState.CompletedSuccess
  --             , contentNumViews = 200
  --             }

  --       _ <- runInsertContent conn initializedContentWithViews
  --       _ <- runInsertContent conn completedContentWithViews

  --       getNextUnprocessed conn `shouldReturn` (Just initializedContentWithViews)

  --   describe "getExpiredActive" $ do
  --     it "should return active content that has expired" $ \conn -> do
  --       now <- getCurrentTime
  --       let nonExpiredActiveContent' = mkContent Image
  --             (ContentId.fromString "8")
  --             (ContentURI "http://example.com/8")
  --             (read "2016-01-01 00:00:00Z" :: UTCTime)
  --           nonExpiredActiveContent = nonExpiredActiveContent'
  --             { contentState = ContentState.Active
  --             , contentActiveAt =
  --                 Just (subtractUTCTime now (toNominalDiffTime (20 :: Minute)))
  --             }

  --           expiredActiveContent' = mkContent Image
  --             (ContentId.fromString "9")
  --             (ContentURI "http://example.com/9")
  --             (read "2016-01-01 00:00:00Z" :: UTCTime)
  --           expiredActiveContent = expiredActiveContent'
  --             { contentState = ContentState.Active
  --             , contentActiveAt =
  --                 Just (subtractUTCTime now (toNominalDiffTime (35 :: Minute)))
  --             }

  --       _ <- runInsertContent conn nonExpiredActiveContent
  --       _ <- runInsertContent conn expiredActiveContent

  --       getExpiredActive conn `shouldReturn` [expiredActiveContent]
