-- {-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Storage.PostgreSQLSpec
  ( main
  , spec
  ) where

import Test.Hspec (Spec, hspec)


main :: IO ()
main = hspec spec




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
