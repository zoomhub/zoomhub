{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ZoomHub.Types.ContentIdSpec
  ( main
  , spec
  ) where

import Data.Aeson (decode, encode)
import Test.Hspec (Spec, context, describe, hspec, it)
import Test.QuickCheck (Property, elements, listOf, property, suchThat, (==>))
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Instances ()

import           ZoomHub.Types.ContentId   (ContentId, fromString, isValid,
                                            unContentId, validChars)

instance Arbitrary ContentId where
  arbitrary = do
    validId <- suchThat (listOf . elements $ validChars) isValid
    return $ fromString validId

prop_invertible :: ContentId -> Property
prop_invertible x = isValid (unContentId x) ==> (decode . encode) x == Just x

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "encode" $
    context "when used with valid strings, i.e. no underscores," $
      it "is inverse to `decode`" $ property prop_invertible
