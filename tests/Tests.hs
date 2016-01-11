-- Example setup: https://git.io/vubDF

module Main (main) where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified ZoomHub.StorageTest as StorageTest
import qualified ZoomHub.Types.Internal.ContentIdTest as ContentIdTest


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
      testGroup "Storage" [
        testProperty "inversion" StorageTest.propInversion
      ]
    , testGroup "ContentId" [
        testProperty "inversion" ContentIdTest.propInversion
      ]
  ]
