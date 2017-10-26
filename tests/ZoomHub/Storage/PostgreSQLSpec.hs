{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Storage.PostgreSQLSpec
  ( main
  , spec
  ) where

import           Control.Exception          (bracket)
import           Data.Maybe                 (fromJust)
import qualified Database.PostgreSQL.Simple as PGS
import           Servant                    (fromText)
import           Test.Hspec                 (Spec, around, describe, hspec, it,
                                             shouldReturn)

import           ZoomHub.Storage.PostgreSQL (ConnectInfo (..),
                                             defaultConnectInfo,
                                             getNextUnprocessed)
import           ZoomHub.Types.Content      (contentNumViews, mkContent)
import qualified ZoomHub.Types.ContentId    as ContentId
import           ZoomHub.Types.ContentType  (ContentType (Image))


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
    describe "getNextUnprocessed" $
      it "should return most viewed item that hasn’t been converted" $ \conn -> do
        -- TODO: Create database state instead of relying on import:
        let content = mkContent Image
                        (ContentId.fromString "6")
                        (fromJust . fromText $ "http://example.com/6/initialized.jpg")
                        (read "2017-01-01 00:00:00Z")
            contentWithViews = content { contentNumViews = 100 }

        getNextUnprocessed conn `shouldReturn` (Just contentWithViews)
