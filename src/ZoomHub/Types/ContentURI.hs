{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ZoomHub.Types.ContentURI
  ( ContentURI
  , ContentURI'(ContentURI)
  -- TODO: Can we test this without exporting it?
  , unContentURI
  ) where

import Data.Aeson (ToJSON, Value(String), toJSON)
import Data.Text (Text)
import qualified Data.Text as T
import Servant (FromHttpApiData, parseUrlPiece)
import Squeal.PostgreSQL (FromValue(..), PG, PGType(PGtext), ToParam(..))

newtype ContentURI' a = ContentURI { unContentURI :: a }
  deriving (Eq, Functor)
type ContentURI = ContentURI' Text

instance Show ContentURI where
  show = T.unpack . unContentURI

-- Text
instance FromHttpApiData ContentURI where
  parseUrlPiece t
    | "http://" `T.isPrefixOf` t             = Right $ ContentURI t
    | "https://" `T.isPrefixOf` t            = Right $ ContentURI t
    | "zoomit://thumbnail/" `T.isPrefixOf` t = Right $ ContentURI t
    | otherwise = Left "Invalid content URI"

-- JSON
instance ToJSON ContentURI where
  toJSON = String . unContentURI

-- Squeal / PostgreSQL
type instance PG ContentURI = 'PGtext
instance ToParam ContentURI 'PGtext where
  toParam = toParam . unContentURI

instance FromValue 'PGtext ContentURI where
  fromValue = ContentURI <$> fromValue @'PGtext
