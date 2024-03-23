{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ZoomHub.Types.ContentIdSpec
  ( main,
    spec,
  )
where

import Data.Aeson (decode, encode)
import Test.Hspec (Spec, context, describe, hspec, it)
import Test.QuickCheck (Property, elements, listOf, property, suchThatMap, (==>))
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Instances ()
import ZoomHub.Types.ContentId
  ( ContentId,
    isValid,
    unContentId,
    validChars,
  )
import qualified ZoomHub.Types.ContentId as ContentId

instance Arbitrary ContentId where
  arbitrary =
    (listOf . elements $ validChars) `suchThatMap` ContentId.fromString

prop_invertible :: ContentId -> Property
prop_invertible x = isValid (unContentId x) ==> (decode . encode) x == Just x

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "encode" do
    context "when used with valid strings, i.e. no underscores," do
      it "is inverse to `decode`" $
        property prop_invertible
