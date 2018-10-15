{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

module ZoomHub.Types.ContentType
  ( ContentType(..)
  , ContentTypeColumn
  ) where

import           Data.Int                             (Int32)
import           Data.Profunctor.Product.Default      (Default, def)
import qualified Database.PostgreSQL.Simple.FromField as PGS
import           Database.SQLite.Simple               (SQLData (SQLInteger))
import           Database.SQLite.Simple.FromField     (FromField, ResultError (ConversionFailed),
                                                       fromField, returnError)
import           Database.SQLite.Simple.Internal      (Field (Field))
import           Database.SQLite.Simple.Ok            (Ok (Ok))
import           Database.SQLite.Simple.ToField       (ToField, toField)
import           Opaleye                              (Column,
                                                       Constant (Constant),
                                                       PGInt4,
                                                       QueryRunnerColumnDefault,
                                                       fieldQueryRunnerColumn,
                                                       pgInt4,
                                                       queryRunnerColumnDefault)
import           Squeal.PostgreSQL                    (FromValue (..),
                                                       PGType (PGint4), PG, ToParam(..))

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

-- Squeal / PostgreSQL
fromPGint4 :: Int32 -> ContentType
fromPGint4 0 = Unknown
fromPGint4 1 = Image
fromPGint4 2 = Webpage
fromPGint4 3 = FlickrImage
fromPGint4 6 = GigaPan
fromPGint4 7 = Zoomify
fromPGint4 10 = PDF10
fromPGint4 11 = PDF11
fromPGint4 14 = WebpageThumbnail
fromPGint4 t = error $ "Invalid ContentType: " <> show t

type instance PG ContentType = 'PGint4
instance ToParam ContentType 'PGint4 where
  toParam Unknown = toParam (0 :: Int32)
  toParam Image = toParam (1 :: Int32)
  toParam Webpage = toParam (2 :: Int32)
  toParam FlickrImage = toParam (3 :: Int32)
  toParam GigaPan = toParam (6 :: Int32)
  toParam Zoomify = toParam (7 :: Int32)
  toParam PDF10 = toParam (10 :: Int32)
  toParam PDF11 = toParam (11 :: Int32)
  toParam WebpageThumbnail = toParam (14 :: Int32)

instance FromValue 'PGint4 ContentType where
  -- TODO: What if database value is not a valid?
  fromValue = fromPGint4 <$> fromValue @'PGint4
