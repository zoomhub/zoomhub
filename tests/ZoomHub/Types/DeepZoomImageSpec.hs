{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.DeepZoomImageSpec
  ( main
  , spec
  ) where

import           Test.Hspec                  (Spec, describe, hspec, it,
                                              shouldBe)

import           ZoomHub.Types.DeepZoomImage (TileFormat (JPEG, PNG), TileOverlap (TileOverlap0, TileOverlap1), TileSize (TileSize254, TileSize256),
                                              fromXML, mkDeepZoomImage)

jpegXML :: String
jpegXML =
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
  \<Image xmlns=\"http://schemas.microsoft.com/deepzoom/2008\"\
  \  Format=\"jpeg\"\
  \  Overlap=\"1\"\
  \  TileSize=\"254\"\
  \  >\
  \  <Size \
  \    Height=\"5678\"\
  \    Width=\"1234\"\
  \  />\
  \</Image>"

pngXML :: String
pngXML =
  "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\
  \<Image xmlns=\"http://schemas.microsoft.com/deepzoom/2008\"\
  \  Format=\"png\"\
  \  Overlap=\"0\"\
  \  TileSize=\"256\"\
  \  >\
  \  <Size \
  \    Height=\"9012\"\
  \    Width=\"3456\"\
  \  />\
  \</Image>"


main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "fromXML" $ do
    it "should create `DeepZoomImage` value from JPEG DZI XML" $
      fromXML jpegXML
      `shouldBe`
      Just (mkDeepZoomImage 1234 5678 TileSize254 TileOverlap1 JPEG)

    it "should create `DeepZoomImage` value from PNG DZI XML" $
      fromXML pngXML
      `shouldBe`
      Just (mkDeepZoomImage 3456 9012 TileSize256 TileOverlap0 PNG)
