{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.Internal.ContentId
  ( ContentId
  , fromInteger
  , fromString
  , unId
  ) where

import           Prelude           hiding (fromInteger)

import           Data.Aeson        (FromJSON, ToJSON, genericParseJSON,
                                    genericToJSON, parseJSON, toJSON)
import           Data.Aeson.Casing (aesonPrefix, camelCase)
import           Data.List         (isInfixOf)
import qualified Data.Text         as T
import           GHC.Generics      (Generic)
import           Servant           (FromText, fromText)


-- TODO: Use record syntax, i.e. `ContentId { unId :: .tring}` without
-- introducing `{"id": <id>}` JSON serialization:
newtype ContentId = ContentId String
  deriving (Eq, Generic, Show)

instance FromText ContentId where
  fromText t = Just $ ContentId $ T.unpack t

instance ToJSON ContentId where
   toJSON = genericToJSON $ aesonPrefix camelCase
instance FromJSON ContentId where
   parseJSON = genericParseJSON $ aesonPrefix camelCase

unId :: ContentId -> String
unId (ContentId cId) = cId

fromInteger :: (Integer -> String) -> Integer -> ContentId
fromInteger encode intId = fromString $ encode intId

-- TODO: Change return type to `Maybe ContentId` to make it a total function:
fromString :: String -> ContentId
fromString cId
  | "_" `isInfixOf` cId = error "Content IDs cannot have underscores (`_`)"
  | otherwise           = ContentId cId
