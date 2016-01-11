{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.Internal.ContentIdTest where

import Data.Aeson (encode, decode)
import Test.QuickCheck ((==>), Property)
import Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import Test.QuickCheck.Instances()
import ZoomHub.Types.Internal.ContentId (ContentId, fromLBS, unContentId)

instance Arbitrary ContentId where
  arbitrary = arbitrary >>= \c -> return . fromLBS $ c

propInversion :: ContentId -> Property
propInversion x = (not . null $ unContentId x) ==> decode (encode x) == Just x
