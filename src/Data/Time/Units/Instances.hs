{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.Time.Units.Instances where

import Data.Aeson ((.=), ToJSON, object, toJSON)
import Data.Text (Text)
import Data.Time.Units (Millisecond, Minute, Second)

instance ToJSON Millisecond where
  toJSON t = object ["value" .= toInteger t, "unit" .= ("ms" :: Text)]

instance ToJSON Second where
  toJSON t = object ["value" .= toInteger t, "unit" .= ("s" :: Text)]

instance ToJSON Minute where
  toJSON t = object ["value" .= toInteger t, "unit" .= ("min" :: Text)]
