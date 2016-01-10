-- Example setup: https://git.io/vubDF

module Main (main) where

import Test.Framework (defaultMain, testGroup, Test)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import qualified ZoomHub.StorageTest as StorageTest


main :: IO ()
main = defaultMain tests

tests :: [Test]
tests = [
    testGroup "Storage" [
      testProperty "inversion" StorageTest.prop_inversion
    ]
  ]
