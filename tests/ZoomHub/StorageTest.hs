module ZoomHub.StorageTest where

import Data.List (isInfixOf)
import Test.QuickCheck ((==>), Property)
import ZoomHub.Storage

prop_inversion :: String -> Property
prop_inversion x = not ("_" `isInfixOf` x) ==> (toId . toFilename) x == x
