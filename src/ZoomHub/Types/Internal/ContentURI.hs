{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.Internal.ContentURI
  ( ContentURI
  , unContentURI
  ) where

import           Data.Aeson (ToJSON, Value (String), toJSON)
import           Data.Text  (Text)
import qualified Data.Text  as T
import           Servant    (FromText, fromText)


-- TODO: Use record syntax, i.e. `ContentURI { unContentURI :: URI }` without
-- introducing `{"contentURI": <contentURI>}` JSON serialization:
newtype ContentURI = ContentURI Text deriving Eq

unContentURI :: ContentURI -> Text
unContentURI (ContentURI uri) = uri

instance Show ContentURI where
  show = T.unpack . unContentURI

-- Text
instance FromText ContentURI where
  fromText t
    | "http://"  `T.isPrefixOf` t = Just (ContentURI t)
    | "https://" `T.isPrefixOf` t = Just (ContentURI t)
    | otherwise = Nothing

-- JSON
instance ToJSON ContentURI where
  toJSON = String . unContentURI
