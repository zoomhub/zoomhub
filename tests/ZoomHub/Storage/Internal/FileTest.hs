module ZoomHub.Storage.Internal.FileTest where

import           Data.List                     (isInfixOf)
import           Test.QuickCheck               (Property, (==>))
import           ZoomHub.Storage.Internal.File (toFilename, toId)


prop_invertible :: String -> Property
prop_invertible x = not ("_" `isInfixOf` x) ==> (toId . toFilename) x == x
