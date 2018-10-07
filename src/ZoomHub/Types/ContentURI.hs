{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}

module ZoomHub.Types.ContentURI
  ( ContentURI
  , ContentURI'(ContentURI)
  , ContentURIColumn
  , pContentURI
  , toColumn
  -- TODO: Can we test this without exporting it?
  , unContentURI
  ) where

import           Data.Aeson                           (ToJSON, Value (String),
                                                       toJSON)
import           Data.Profunctor.Product.TH           (makeAdaptorAndInstance)
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import qualified Database.PostgreSQL.Simple.FromField as PGS
import           Database.SQLite.Simple               (SQLData (SQLText))
import           Database.SQLite.Simple.FromField     (FromField, ResultError (ConversionFailed),
                                                       fromField, returnError)
import           Database.SQLite.Simple.Internal      (Field (Field))
import           Database.SQLite.Simple.Ok            (Ok (Ok))
import           Database.SQLite.Simple.ToField       (ToField, toField)
import           Opaleye                              (Column, PGText,
                                                       QueryRunnerColumnDefault,
                                                       fieldQueryRunnerColumn,
                                                       pgStrictText,
                                                       queryRunnerColumnDefault)
import           Servant                              (FromHttpApiData,
                                                       parseUrlPiece)
import Squeal.PostgreSQL (FromValue(..), PGType(PGtext))

newtype ContentURI' a = ContentURI { unContentURI :: a }
  deriving (Eq)
type ContentURI = ContentURI' Text

instance Show ContentURI where
  show = T.unpack . unContentURI

-- Functor
instance Functor ContentURI' where
  fmap f (ContentURI a) = ContentURI (f a)

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

-- SQLite
instance ToField ContentURI where
  toField = SQLText . unContentURI

instance FromField ContentURI where
  fromField field@(Field (SQLText t) _) =
    case parseUrlPiece t of
      Right r -> Ok r
      Left e  -> returnError ConversionFailed field (T.unpack e)
  fromField field = returnError ConversionFailed field "Invalid content URI"

-- PostgreSQL
type ContentURIColumn = ContentURI' (Column PGText)
$(makeAdaptorAndInstance "pContentURI" ''ContentURI')

toColumn :: ContentURI -> ContentURIColumn
toColumn = fmap pgStrictText

instance PGS.FromField ContentURI where
  fromField f mdata = PGS.fromField f mdata >>= parseContentURI
    where
      parseContentURI t = case parseUrlPiece t of
        Right r -> return r
        Left _  -> PGS.returnError PGS.ConversionFailed f "Invalid content URI"

instance QueryRunnerColumnDefault PGText ContentURI where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

-- Squeal / PostgreSQL
instance FromValue 'PGtext ContentURI where
  fromValue = ContentURI <$> fromValue @'PGtext
