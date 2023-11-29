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

module ZoomHub.Types.ContentURI
  ( ContentURI,
    -- TODO: Can we test this without exporting it?
    unContentURI,
  )
where

import Data.Aeson (ToJSON, Value (String), toJSON)
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Data.Text as T
import Servant (FromHttpApiData, parseUrlPiece)
import Squeal.PostgreSQL (FromValue (..), PG, PGType (PGtext), ToParam (..), IsPG, ToPG, FromPG(..), Inline)

newtype ContentURI = ContentURI { unContentURI :: Text }
  deriving stock (Eq, Generic)
  deriving newtype (IsPG, ToPG db, Inline)


instance Show ContentURI where
  show = T.unpack . unContentURI

-- Text
instance FromHttpApiData ContentURI where
  parseUrlPiece t
    | "http://" `T.isPrefixOf` t = Right $ ContentURI t
    | "https://" `T.isPrefixOf` t = Right $ ContentURI t
    | "zoomit://thumbnail/" `T.isPrefixOf` t = Right $ ContentURI t
    | otherwise = Left "Invalid content URI"

-- JSON
instance ToJSON ContentURI where
  toJSON = String . unContentURI

-- Squeal / PostgreSQL
-- type instance PG ContentURI = 'PGtext



instance FromPG ContentURI where
  fromPG = ContentURI <$> fromPG @Text
