module ZoomHub.StorageTest where

import Data.List (isInfixOf)
import Test.QuickCheck ((==>), Property)
import ZoomHub.Storage

propInversion :: String -> Property
propInversion x = not ("_" `isInfixOf` x) ==> (toId . toFilename) x == x
