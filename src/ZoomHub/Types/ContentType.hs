{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}

module ZoomHub.Types.ContentType
  ( ContentType(..)
  , ContentTypeColumn
  ) where

import           Database.SQLite.Simple               (SQLData (SQLInteger))
import           Database.SQLite.Simple.FromField     (FromField, ResultError (ConversionFailed),
                                                       fromField, returnError)
import           Database.SQLite.Simple.Internal      (Field (Field))
import           Database.SQLite.Simple.Ok            (Ok (Ok))
import           Database.SQLite.Simple.ToField       (ToField, toField)
import           Data.Profunctor.Product.Default      (Default, def)
import qualified Database.PostgreSQL.Simple.FromField as PGS
import           Opaleye                              (Column,
                                                       Constant (Constant),
                                                       PGInt4,
                                                       QueryRunnerColumnDefault,
                                                       fieldQueryRunnerColumn,
                                                       pgInt4,
                                                       queryRunnerColumnDefault)
data ContentType =
    Unknown
  | Image
  | Webpage
  | FlickrImage
  | GigaPan
  | Zoomify
  | PDF10
  | PDF11
  | WebpageThumbnail
  deriving (Eq, Show)

-- SQLite
instance ToField ContentType where
  toField Unknown          = SQLInteger  0
  toField Image            = SQLInteger  1
  toField Webpage          = SQLInteger  2
  toField FlickrImage      = SQLInteger  3
  toField GigaPan          = SQLInteger  6
  toField Zoomify          = SQLInteger  7
  toField PDF10            = SQLInteger 10
  toField PDF11            = SQLInteger 11
  toField WebpageThumbnail = SQLInteger 14

instance FromField ContentType where
  fromField (Field (SQLInteger  0) _) = Ok Unknown
  fromField (Field (SQLInteger  1) _) = Ok Image
  fromField (Field (SQLInteger  2) _) = Ok Webpage
  fromField (Field (SQLInteger  3) _) = Ok FlickrImage
  fromField (Field (SQLInteger  6) _) = Ok GigaPan
  fromField (Field (SQLInteger  7) _) = Ok Zoomify
  fromField (Field (SQLInteger 10) _) = Ok PDF10
  fromField (Field (SQLInteger 11) _) = Ok PDF11
  fromField (Field (SQLInteger 14) _) = Ok WebpageThumbnail
  fromField f = returnError ConversionFailed f "invalid content type"

-- PostgreSQL
type ContentTypeColumn = Column PGInt4

instance PGS.FromField ContentType where
  fromField f mdata = PGS.fromField f mdata >>= parseContentType
    where
      parseContentType :: Maybe Integer -> PGS.Conversion ContentType
      parseContentType r = case r of
        Just 0  -> return Unknown
        Just 1  -> return Image
        Just 2  -> return Webpage
        Just 3  -> return FlickrImage
        Just 6  -> return GigaPan
        Just 7  -> return Zoomify
        Just 10 -> return PDF10
        Just 11 -> return PDF11
        Just 14 -> return WebpageThumbnail
        Just _  -> PGS.returnError PGS.ConversionFailed f "unrecognized content type"
        Nothing -> PGS.returnError PGS.UnexpectedNull f "empty content type"

instance QueryRunnerColumnDefault PGInt4 ContentType where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance Default Constant ContentType ContentTypeColumn where
  def = Constant def'
    where
      def' :: ContentType -> ContentTypeColumn
      def' Unknown          = pgInt4 0
      def' Image            = pgInt4 1
      def' Webpage          = pgInt4 2
      def' FlickrImage      = pgInt4 3
      def' GigaPan          = pgInt4 6
      def' Zoomify          = pgInt4 7
      def' PDF10            = pgInt4 10
      def' PDF11            = pgInt4 11
      def' WebpageThumbnail = pgInt4 14
