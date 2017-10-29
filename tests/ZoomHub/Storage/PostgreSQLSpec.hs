{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Storage.PostgreSQLSpec
  ( main
  , spec
  ) where

import           Control.Exception          (bracket)
import qualified Database.PostgreSQL.Simple as PGS
import           Test.Hspec                 (Spec, around, describe, hspec, it,
                                             shouldReturn)

import           ZoomHub.Storage.PostgreSQL (ConnectInfo (..),
                                             defaultConnectInfo,
                                             getNextUnprocessed,
                                             runInsertContent)
import           ZoomHub.Types.Content      (contentNumViews, contentState,
                                             mkContent)
import qualified ZoomHub.Types.ContentId    as ContentId
import qualified ZoomHub.Types.ContentState as ContentState
import           ZoomHub.Types.ContentType  (ContentType (Image))
import           ZoomHub.Types.ContentURI   (ContentURI' (ContentURI))


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
spec =
  around withDatabaseConnection $
    describe "getNextUnprocessed" $ do
      it "should return most viewed item that hasn’t been converted" $ \conn -> do
        let c1 = mkContent Image
                        (ContentId.fromString "6")
                        (ContentURI "http://example.com/6/initialized.jpg")
                        (read "2017-01-01 00:00:00Z")
            c1WithViews = c1
              { contentNumViews = 100
              }

            c2 = mkContent Image
                        (ContentId.fromString "7")
                        (ContentURI  "http://example.com/7/initialized.jpg")
                        (read "2016-01-01 00:00:00Z")
            c2WithViews = c2
              { contentState = ContentState.CompletedSuccess
              , contentNumViews = 200
              }

        _ <- runInsertContent conn c1WithViews
        _ <- runInsertContent conn c2WithViews

        getNextUnprocessed conn `shouldReturn` (Just c1WithViews)
