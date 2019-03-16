{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Storage.PostgreSQL2Spec
  ( main
  , spec
  ) where

import Control.Exception (bracket)
import Control.Monad (void)
import Data.Time.Clock (UTCTime)
import qualified Generics.SOP as SOP
import Squeal.PostgreSQL (Connection, connectdb, define, finish, runPQ)
import Test.Hspec (Spec, around, describe, hspec, it, shouldBe)

import ZoomHub.Storage.PostgreSQL2 (create, getById, getById')
import ZoomHub.Storage.PostgreSQL2.Schema (Schema, setup, teardown)
import ZoomHub.Types.Content (contentDZI, contentNumViews, mkContent)
import ZoomHub.Types.ContentId (ContentId)
import qualified ZoomHub.Types.ContentId as ContentId
import ZoomHub.Types.ContentType (ContentType(Image))
import ZoomHub.Types.ContentURI (ContentURI'(ContentURI))
import ZoomHub.Types.DeepZoomImage (mkDeepZoomImage)
import ZoomHub.Types.DeepZoomImage.TileFormat (TileFormat(..))
import ZoomHub.Types.DeepZoomImage.TileOverlap (TileOverlap(..))
import ZoomHub.Types.DeepZoomImage.TileSize (TileSize(..))

main :: IO ()
main = hspec spec

withDatabaseConnection :: (SOP.K Connection Schema -> IO a) -> IO a
withDatabaseConnection = bracket acquire release
  where
    acquire :: IO (SOP.K Connection Schema)
    acquire = do
      conn <- connectdb "host=localhost port=5432 dbname=zoomhub_test"
      (_, conn') <- runPQ (define setup) conn
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
        let initializedContent = mkContent Image
                defaultContentId
                (ContentURI "http://example.com/6")
                (read "2018-10-16 00:00:00Z" :: UTCTime)
            dzi = mkDeepZoomImage 400 300 TileSize254 TileOverlap1 PNG
            initializedContentWithViews = initializedContent
              { contentNumViews = 100
              , contentDZI = Just dzi
              }
        void $ runPQ (create initializedContentWithViews) conn
        (result, _) <- runPQ (getById defaultContentId) conn
        result `shouldBe` (Just initializedContentWithViews)

    describe "getById'" $
      it "should increase number of views" $ \conn -> do
        let initializedContent = mkContent Image
                defaultContentId
                (ContentURI "http://example.com/6")
                (read "2018-10-16 00:00:00Z" :: UTCTime)
            dzi = mkDeepZoomImage 400 300 TileSize254 TileOverlap1 PNG
            initializedContentWithViews = initializedContent
              { contentNumViews = 1
              , contentDZI = Just dzi
              }
        void $ runPQ (create initializedContentWithViews) conn
        void $ runPQ (getById' defaultContentId) conn
        void $ runPQ (getById' defaultContentId) conn
        (result, _) <- runPQ (getById defaultContentId) conn
        result `shouldBe` (Just (initializedContentWithViews { contentNumViews = 3 }))

defaultContentId :: ContentId
defaultContentId = ContentId.fromString "8"
