{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

module ZoomHub.Types.ContentMIME
  ( ContentMIME
  , ContentMIME'(ContentMIME)
  , unContentMIME
  , pContentMIME
  ) where

import           Codec.MIME.Parse                     (parseMIMEType)
import           Codec.MIME.Type                      (Type, showType)
import           Data.Aeson                           (ToJSON, Value (String),
                                                       toJSON)
import           Data.Profunctor.Product.TH           (makeAdaptorAndInstance)
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
                                                       queryRunnerColumnDefault)


newtype ContentMIME' a = ContentMIME { unContentMIME :: a } deriving (Eq, Show)
type ContentMIME = ContentMIME' Type

-- SQLite
instance ToField ContentMIME where
  toField = SQLText . showType . unContentMIME

instance FromField ContentMIME where
  fromField f@(Field (SQLText t) _) =
    case parseMIMEType t of
      Just r  -> Ok (ContentMIME r)
      Nothing -> returnError ConversionFailed f
        ("invalid content MIME type: " ++ T.unpack t)
  fromField f = returnError ConversionFailed f "invalid MIME type"

-- JSON
instance ToJSON ContentMIME where
  toJSON = String . showType . unContentMIME

-- PostgreSQL
type ContentMIMEColumn = ContentMIME' (Column PGText)
$(makeAdaptorAndInstance "pContentMIME" ''ContentMIME')

instance PGS.FromField ContentMIME where
  fromField f mdata = PGS.fromField f mdata >>= parseContentMIME
    where
      parseContentMIME t = case parseMIMEType t of
        Just r  -> return (ContentMIME r)
        Nothing -> PGS.returnError PGS.ConversionFailed f "invalid content MIME type"

instance QueryRunnerColumnDefault PGText ContentMIME where
  queryRunnerColumnDefault = fieldQueryRunnerColumn
