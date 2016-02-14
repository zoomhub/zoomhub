module Main (main) where

import           Test.Hspec                           (context, describe, hspec,
                                                       it)
import           Test.QuickCheck                      (property)

import qualified ZoomHub.Storage.Internal.FileTest    as File
import qualified ZoomHub.Types.Internal.ContentIdTest as ContentId


main :: IO ()
main = hspec $ do
  describe "ZoomHub.Storage.File: `toId`" $
    context "when used with valid strings, i.e. no underscores," $
      it "is inverse to `toFilename`" $ property
        File.prop_invertible

  describe "ZoomHub.Types.Internal.ContentId: `encode`" $
    context "when used with valid strings, i.e. no underscores," $
      it "is inverse to `decode`" $ property
        ContentId.prop_invertible
