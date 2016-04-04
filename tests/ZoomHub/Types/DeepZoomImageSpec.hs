{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.DeepZoomImageSpec
  ( main
  , spec
  ) where

import           Test.Hspec                  (Spec, describe, hspec, it,
                                              shouldBe)

import           ZoomHub.Types.DeepZoomImage (DeepZoomImage, TileFormat (JPEG),
                                              TileOverlap (TileOverlap1),
                                              TileSize (TileSize254), fromXML,
                                              mkDeepZoomImage)

sampleXML :: String
sampleXML =
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
  \<Image xmlns=\"http://schemas.microsoft.com/deepzoom/2008\"\
  \  Format=\"jpeg\"\
  \  Overlap=\"1\"\
  \  TileSize=\"254\"\
  \  >\
  \  <Size \
  \    Height=\"1368\"\
  \    Width=\"1824\"\
  \  />\
  \</Image>"


main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "fromXML" $
    it "should create `DeepZoomImage` value" $
      fromXML sampleXML
      `shouldBe`
      Just (mkDeepZoomImage 1824 1368 TileSize254 TileOverlap1 JPEG)
