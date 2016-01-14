-- Example setup: https://git.io/vubDF

module Main (main) where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified ZoomHub.Storage.FileTest as FileTest
import qualified ZoomHub.Types.Internal.ContentIdTest as ContentIdTest


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
      testGroup "ZoomHub.Storage.File" [
        testProperty "inversion" FileTest.propInversion
      ]
    , testGroup "ZoomHub.Types.Internal.ContentId" [
        testProperty "inversion" ContentIdTest.propInversion
      ]
  ]
