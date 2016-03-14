module ZoomHub.Storage.Internal.FileSpec
  ( main
  , spec
  ) where

import           Data.List                     (isInfixOf)
import           Test.Hspec                    (Spec, context, describe, hspec,
                                                it)
import           Test.QuickCheck               (Property, property, (==>))

import           ZoomHub.Storage.Internal.File (toFilename, toId)

prop_invertible :: String -> Property
prop_invertible x = not ("_" `isInfixOf` x) ==> (toId . toFilename) x == x

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "toId" $
    context "when used with valid strings, i.e. no underscores," $
      it "is inverse to `toFilename`" $ property prop_invertible
