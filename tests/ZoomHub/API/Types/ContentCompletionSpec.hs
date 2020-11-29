{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.API.Types.ContentCompletionSpec
  ( main,
    spec,
  )
where

import Data.Aeson (decode)
import qualified Data.ByteString.Lazy as LB
import Data.Maybe (Maybe (..), fromJust)
import Network.URI (parseAbsoluteURI)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import ZoomHub.API.Types.ContentCompletion (ContentCompletion (..), FailureCompletion (..), SuccessCompletion (..))
import ZoomHub.API.Types.DeepZoomImage (DeepZoomImageURI (..), mkDeepZoomImage)
import qualified ZoomHub.Types.ContentMIME as ContentMIME
import ZoomHub.Types.DeepZoomImage (TileSize (TileSize254))
import ZoomHub.Types.DeepZoomImage (TileFormat (PNG))
import ZoomHub.Types.DeepZoomImage.TileOverlap (TileOverlap (TileOverlap1))

successCompletionJSON :: LB.ByteString
successCompletionJSON =
  "{\"type\":\"success\",\"mime\":\"image/png\",\"size\":1234,\
  \\"dzi\":{\"url\":\"https://example.com/test.dzi\",\"width\":456,\"height\":789,\"tileSize\":254,\
  \\"tileOverlap\":1,\"tileFormat\":\"png\"}}"

failureCompletionJSON :: LB.ByteString
failureCompletionJSON = "{\"type\":\"failure\",\"error\":\"FAIL!\"}"

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "ContentCompletion" do
    describe "SuccessCompletion" do
      it "should decode from JSON" $
        let uri = DeepZoomImageURI . fromJust $ parseAbsoluteURI "https://example.com/test.dzi"
            dzi = mkDeepZoomImage uri 456 789 TileSize254 TileOverlap1 PNG
            mMIME = ContentMIME.fromText "image/png"
         in (decode successCompletionJSON :: Maybe ContentCompletion)
              `shouldBe` ( Just . Success $ SuccessCompletion
                             { scSize = 1234,
                               scMIME = mMIME,
                               scDZI = dzi
                             }
                         )
    describe "FailureCompletion" do
      it "should decode from JSON" $
        (decode failureCompletionJSON :: Maybe ContentCompletion)
          `shouldBe` (Just . Failure $ FailureCompletion {fcErrorMessage = "FAIL!"})
