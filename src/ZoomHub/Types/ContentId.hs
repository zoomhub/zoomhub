{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.ContentId
  ( ContentId
  , fromInteger
  , fromString
  , isValid
  , unId
  , validChars
  ) where

import           Prelude                          hiding (fromInteger)

import           Data.Aeson                       (FromJSON, ToJSON,
                                                   genericParseJSON,
                                                   genericToJSON, parseJSON,
                                                   toJSON)
import           Data.Aeson.Casing                (aesonPrefix, camelCase)
import           Data.List                        (intersperse)
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import qualified Data.Text                        as T
import           Database.SQLite.Simple           (SQLData (SQLText))
import           Database.SQLite.Simple.FromField (FromField, ResultError (ConversionFailed),
                                                   fromField, returnError)
import           Database.SQLite.Simple.Internal  (Field (Field))
import           Database.SQLite.Simple.Ok        (Ok (Ok))
import           Database.SQLite.Simple.ToField   (ToField, toField)
import           GHC.Generics                     (Generic)
import           Servant                          (FromHttpApiData,
                                                   parseUrlPiece)

-- TODO: Use record syntax, i.e. `ContentId { unId :: String }` without
-- introducing `{"id": <id>}` JSON serialization:
newtype ContentId =
  ContentId String
  deriving (Eq, Generic, Show)

unId :: ContentId -> String
unId (ContentId cId) = cId

fromInteger :: (Integer -> String) -> Integer -> ContentId
fromInteger encode intId = fromString $ encode intId

-- TODO: Change return type to `Maybe ContentId` to make it a total function:
fromString :: String -> ContentId
fromString s
  | isValid s = ContentId s
  | otherwise =
    error $
    "Invalid content ID '" ++
    s ++ "'." ++ " Valid characters: " ++ intersperse ',' validChars ++ "."

-- NOTE: Duplicated from `hashids`: https://git.io/vgpT4
-- TODO: Use this for `hashids` initialization.
validChars :: String
validChars = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']

validCharsSet :: Set Char
validCharsSet = Set.fromList validChars

isValid :: String -> Bool
isValid = all (`Set.member` validCharsSet)

-- Text
instance FromHttpApiData ContentId where
  parseUrlPiece t
    | isValid s = Right . fromString $ s
    | otherwise = Left "Invalid content ID"
    where
      s = T.unpack t

-- JSON
instance ToJSON ContentId where
  toJSON = genericToJSON $ aesonPrefix camelCase

instance FromJSON ContentId where
  parseJSON = genericParseJSON $ aesonPrefix camelCase

-- SQLite
instance ToField ContentId where
  toField = SQLText . T.pack . unId

instance FromField ContentId where
  fromField f@(Field (SQLText t) _) =
    case parseUrlPiece t of
      Right r -> Ok r
      Left _  -> returnError ConversionFailed f "Invalid content ID"
  fromField f = returnError ConversionFailed f "Invalid content ID"
