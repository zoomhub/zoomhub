{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ZoomHub.Types.ContentId
  ( ContentId,
    fromInteger,
    fromString,
    isValid,
    unContentId,
    validChars,
  )
where

import Data.Aeson
  ( FromJSON(..),
    ToJSON,
    parseJSON,
    toJSON,
  )
import Data.Aeson.Types (typeMismatch)
import qualified Data.Aeson as Aeson
import Data.List (intersperse)
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant (FromHttpApiData, parseUrlPiece)
import Squeal.PostgreSQL (IsPG, ToPG, Inline)
import Prelude hiding (fromInteger)

-- TODO: Use record syntax, i.e. `ContentId { unContentId :: String }` without
-- introducing `{"id": <id>}` JSON serialization:
newtype ContentId = ContentId { unContentId :: String }
  deriving stock (Eq, Generic, Show)
  deriving newtype (IsPG, ToPG db, Inline)

fromInteger :: (Integer -> String) -> Integer -> ContentId
fromInteger encode intId = fromString $ encode intId

-- TODO: Change return type to `Maybe ContentId` to make it a total function:
fromString :: String -> ContentId
fromString s
  | isValid s = ContentId s
  | otherwise = error $
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
  toJSON = Aeson.String . T.pack . unContentId

instance FromJSON ContentId where
  parseJSON (Aeson.String s)
    | isValid (T.unpack s) = pure $ ContentId $ T.unpack s
    | otherwise = fail $ "Invalid ContentId: " <> T.unpack s
  parseJSON invalid = typeMismatch "ContentId" invalid

-- -- Squeal / PostgreSQL
-- type instance PG ContentId = 'PGtext

-- instance ToParam ContentId 'PGtext where
--   toParam = toParam . unContentId

-- instance FromValue 'PGtext ContentId where
--   fromValue = ContentId <$> fromValue @'PGtext
