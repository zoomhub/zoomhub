-- Example setup: https://git.io/vubDF

module Main (main) where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified ZoomHub.Storage.Internal.FileTest as File
import qualified ZoomHub.Types.Internal.ContentIdTest as ContentId


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
      testGroup "ZoomHub.Storage.File" [
        testProperty "inversion" File.propInversion
      ]
    , testGroup "ZoomHub.Types.Internal.ContentId" [
        testProperty "inversion" ContentId.propInversion
      ]
  ]
