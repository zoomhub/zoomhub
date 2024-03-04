{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
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

import Control.Monad.Except (MonadError (throwError))
import Data.Aeson
  ( FromJSON (..),
    ToJSON,
    parseJSON,
    toJSON,
  )
import qualified Data.Aeson as Aeson
import Data.Aeson.Types (typeMismatch)
import Data.Foldable (fold)
import qualified Data.List as List
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant (FromHttpApiData, parseUrlPiece)
import Squeal.PostgreSQL (FromPG (fromPG), Inline (inline), IsPG (PG), PGType (PGtext), ToPG (toPG))
import Prelude hiding (fromInteger)

-- TODO: Use record syntax, i.e. `ContentId { unContentId :: String }` without
-- introducing `{"id": <id>}` JSON serialization:
newtype ContentId = ContentId {unContentId :: String}
  deriving stock (Eq, Generic, Show)

fromInteger :: (Integer -> String) -> Integer -> Maybe ContentId
fromInteger encode intId = fromString $ encode intId

fromString :: String -> Maybe ContentId
fromString s
  | isValid s = Just $ ContentId s
  | otherwise = Nothing

fromText :: Text -> Maybe ContentId
fromText = fromString . T.unpack

toText :: ContentId -> Text
toText = T.pack . unContentId

-- NOTE: Duplicated from `hashids`: https://git.io/vgpT4
-- TODO: Use this for `hashids` initialization.
validChars :: String
validChars = ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9']

validCharsSet :: Set Char
validCharsSet = Set.fromList validChars

isValid :: String -> Bool
isValid = all (`Set.member` validCharsSet)

-- Text
instance FromHttpApiData ContentId where
  parseUrlPiece t =
    case fromText t of
      Just cId -> Right cId
      Nothing ->
        Left $
          fold
            [ "Invalid content ID '" <> t <> "'.",
              " Valid characters: " <> T.pack (List.intersperse ',' validChars) <> "."
            ]

-- JSON
instance ToJSON ContentId where
  toJSON = Aeson.String . T.pack . unContentId

instance FromJSON ContentId where
  parseJSON (Aeson.String s)
    | isValid (T.unpack s) = pure $ ContentId $ T.unpack s
    | otherwise = fail $ "Invalid ContentId: " <> T.unpack s
  parseJSON invalid = typeMismatch "ContentId" invalid

-- Squeal / PostgreSQL
-- TODO: Look into newtype deriving
instance IsPG ContentId where
  type PG ContentId = 'PGtext

instance FromPG ContentId where
  fromPG = do
    value <- fromPG @Text
    case fromText value of
      Just format -> pure format
      Nothing -> throwError $ "Invalid content ID: \"" <> value <> "\""

instance ToPG db ContentId where
  toPG = toPG . toText

instance Inline ContentId where
  inline = inline . toText
