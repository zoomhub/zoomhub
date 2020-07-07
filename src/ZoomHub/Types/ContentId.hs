{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ZoomHub.Types.ContentId
  ( ContentId,
    ContentId',
    fromInteger,
    fromString,
    isValid,
    mkContentId,
    unContentId,
    validChars,
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON,
    genericParseJSON,
    genericToJSON,
    parseJSON,
    toJSON,
  )
import Data.Aeson.Casing (aesonPrefix, camelCase)
import Data.List (intersperse)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant (FromHttpApiData, parseUrlPiece)
import Squeal.PostgreSQL (FromValue (..), PG, PGType (PGtext), ToParam (..))
import Prelude hiding (fromInteger)

-- TODO: Use record syntax, i.e. `ContentId { unContentId :: String }` without
-- introducing `{"id": <id>}` JSON serialization:
newtype ContentId' a = ContentId a
  deriving (Eq, Functor, Generic, Show)

type ContentId = ContentId' String

unContentId :: ContentId -> String
unContentId (ContentId cId) = cId

mkContentId :: a -> ContentId' a
mkContentId = ContentId

fromInteger :: (Integer -> String) -> Integer -> ContentId
fromInteger encode intId = fromString $ encode intId

-- TODO: Change return type to `Maybe ContentId` to make it a total function:
fromString :: String -> ContentId
fromString s
  | isValid s = ContentId s
  | otherwise =
    error $
      "Invalid content ID '" ++ s ++ "'."
        ++ " Valid characters: "
        ++ intersperse ',' validChars
        ++ "."

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

-- Squeal / PostgreSQL
type instance PG ContentId = 'PGtext

instance ToParam ContentId 'PGtext where
  toParam = toParam . unContentId

instance FromValue 'PGtext ContentId where
  fromValue = ContentId <$> fromValue @'PGtext
