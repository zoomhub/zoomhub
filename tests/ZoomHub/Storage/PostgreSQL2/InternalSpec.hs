{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module ZoomHub.Storage.PostgreSQL2.InternalSpec
  ( main
  , spec
  ) where

import Control.Exception (bracket)
import Data.Time.Clock (getCurrentTime)
import Data.Int (Int64)
import qualified Generics.SOP as SOP
import Squeal.PostgreSQL
  ( Connection
  , connectdb
  , define
  , finish
  , runPQ
  )
import Test.Hspec (Spec, around, describe, hspec, it, shouldBe)

import ZoomHub.Storage.PostgreSQL.Schema (Schema, setup, teardown)
import ZoomHub.Storage.PostgreSQL2.Internal (createImage, getImageById)
import ZoomHub.Types.DeepZoomImage (mkDeepZoomImage)
import ZoomHub.Types.DeepZoomImage.TileSize (TileSize(..))
import ZoomHub.Types.DeepZoomImage.TileOverlap (TileOverlap(..))
import ZoomHub.Types.DeepZoomImage.TileFormat (TileFormat(..))

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
  around withDatabaseConnection $
    describe "createImage" $
      it "should create new Deep Zoom image" $ \conn -> do
        let cid = 6 :: Int64
            image = mkDeepZoomImage 400 300 TileSize254 TileOverlap1 PNG
        initializedAt <- getCurrentTime
        _ <- runPQ (createImage cid initializedAt image) conn
        (result, _) <- runPQ (getImageById cid) conn
        result `shouldBe` (Just image)
