{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ZoomHub.Types.Internal.ContentIdTest where

import           Data.Aeson                       (decode, encode)
import           Test.QuickCheck                  (elements, listOf)
import           Test.QuickCheck.Arbitrary        (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances        ()
import           ZoomHub.Types.Internal.ContentId (ContentId, fromString)


instance Arbitrary ContentId where
  arbitrary = do
    validId <- listOf $ elements $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']
    return $ fromString validId

prop_invertible :: ContentId -> Bool
prop_invertible x = (decode . encode) x == Just x
