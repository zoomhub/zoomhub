{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.Internal.ContentURI
  ( ContentURI
  , unContentURI
  ) where

import           Data.Aeson  (ToJSON, Value (String), toJSON)
import qualified Data.Text   as T
import           Network.URI (URI, parseAbsoluteURI)
import           Servant     (FromText, fromText)


-- TODO: Use record syntax, i.e. `ContentURI { unContentURI :: URI }` without
-- introducing `{"contentURI": <contentURI>}` JSON serialization:
newtype ContentURI = ContentURI URI deriving Eq

unContentURI :: ContentURI -> URI
unContentURI (ContentURI uri) = uri

instance Show ContentURI where
  show = show . unContentURI

-- Text
instance FromText ContentURI where
  fromText t = parseAbsoluteURI (T.unpack t) >>= Just . ContentURI

-- JSON
instance ToJSON ContentURI where
  toJSON = String . T.pack . show . unContentURI
