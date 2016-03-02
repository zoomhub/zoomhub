{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ZoomHub.Types.Internal.ContentIdTest where

import           Data.Aeson                       (decode, encode)
import           Test.QuickCheck                  (Property, elements, listOf,
                                                   suchThat, (==>))
import           Test.QuickCheck.Arbitrary        (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances        ()
import           ZoomHub.Types.Internal.ContentId (ContentId, fromString,
                                                   isValid, unId, validChars)

instance Arbitrary ContentId where
  arbitrary = do
    validId <- suchThat (listOf . elements $ validChars) isValid
    return $ fromString validId

prop_invertible :: ContentId -> Property
prop_invertible x = (isValid $ unId x) ==> (decode . encode) x == Just x
