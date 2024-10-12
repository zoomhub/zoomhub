{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ZoomHub.Types.ContentURI
  ( ContentURI (ContentURI),
    fromText,
    -- TODO: Can we test this without exporting it?
    unContentURI,
  )
where

import Control.Monad.Except (MonadError (throwError))
import Data.Aeson (ToJSON, Value (String), toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Servant (FromHttpApiData, parseUrlPiece)
import Squeal.PostgreSQL (FromPG (..), Inline (inline), IsPG (PG), PGType (PGtext), ToPG (toPG))

newtype ContentURI = ContentURI {unContentURI :: Text}
  deriving stock (Eq, Generic)

instance Show ContentURI where
  show = show . unContentURI

fromText :: Text -> Maybe ContentURI
fromText t
  | "http://" `T.isPrefixOf` t = Just $ ContentURI t
  | "https://" `T.isPrefixOf` t = Just $ ContentURI t
  | "zoomit://thumbnail/" `T.isPrefixOf` t = Just $ ContentURI t
  | otherwise = Nothing

-- Text
instance FromHttpApiData ContentURI where
  parseUrlPiece t = case fromText t of
    Just cURI -> Right cURI
    Nothing -> Left "Invalid content URI"

-- JSON
instance ToJSON ContentURI where
  toJSON = String . unContentURI

-- Squeal / PostgreSQL
-- TODO: Look into newtype deriving
instance IsPG ContentURI where
  type PG ContentURI = 'PGtext

instance ToPG db ContentURI where
  toPG = toPG . unContentURI

instance FromPG ContentURI where
  fromPG = do
    value <- fromPG @Text
    case fromText value of
      Just format -> pure format
      Nothing -> throwError $ "Invalid content ID: \"" <> value <> "\""

instance Inline ContentURI where
  inline = inline . unContentURI
