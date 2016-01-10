module ZoomHub.StorageTest where

import Test.QuickCheck
import ZoomHub.Storage

prop_inversion :: String -> Property
prop_inversion x = not (x == "_") ==> (toId . toFilename) x == x
