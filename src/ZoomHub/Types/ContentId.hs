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
import qualified Data.Set                         as S
import qualified Data.Text                        as T
import           Database.SQLite.Simple           (SQLData (SQLText))
import           Database.SQLite.Simple.FromField (FromField, ResultError (ConversionFailed),
                                                   fromField, returnError)
import           Database.SQLite.Simple.Internal  (Field (Field))
import           Database.SQLite.Simple.Ok        (Ok (Ok))
import           Database.SQLite.Simple.ToField   (ToField, toField)
import           GHC.Generics                     (Generic)
import           Servant                          (FromText, fromText)



-- TODO: Use record syntax, i.e. `ContentId { unId :: String }` without
-- introducing `{"id": <id>}` JSON serialization:
newtype ContentId = ContentId String
  deriving (Eq, Generic, Show)

unId :: ContentId -> String
unId (ContentId cId) = cId

fromInteger :: (Integer -> String) -> Integer -> ContentId
fromInteger encode intId = fromString $ encode intId

-- TODO: Change return type to `Maybe ContentId` to make it a total function:
fromString :: String -> ContentId
fromString s
  | isValid s = ContentId s
  | otherwise = error $ "Invalid content ID '" ++ s ++ "'." ++
    " Valid characters: " ++ intersperse ',' validChars ++ "."

-- NOTE: Duplicated from `hashids`: https://git.io/vgpT4
-- TODO: Use this for `hashids` initialization.
validChars :: String
validChars = ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']

validCharsSet :: S.Set Char
validCharsSet = S.fromList validChars

isValid :: String -> Bool
isValid = all (`S.member` validCharsSet)

-- Text
instance FromText ContentId where
  fromText t
    | isValid s = Just . fromString $ s
    | otherwise = Nothing
    where s = T.unpack t

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
    case fromText t of
      Just r  -> Ok r
      Nothing -> returnError ConversionFailed f "Invalid `ContentId`"
  fromField f = returnError ConversionFailed f "Invalid `ContentId`"