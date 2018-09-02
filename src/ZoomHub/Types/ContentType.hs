{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Types.ContentType
  ( ContentType(..)
  ) where

import           Database.SQLite.Simple           (SQLData (SQLInteger))
import           Database.SQLite.Simple.FromField (FromField, ResultError (ConversionFailed),
                                                   fromField, returnError)
import           Database.SQLite.Simple.Internal  (Field (Field))
import           Database.SQLite.Simple.Ok        (Ok (Ok))
import           Database.SQLite.Simple.ToField   (ToField, toField)

data ContentType
  = Unknown
  | Image
  | Webpage
  | FlickrImage
  | GigaPan
  | Zoomify
  | PDF10
  | PDF11
  | WebpageThumbnail
  deriving (Eq, Show)

instance ToField ContentType where
  toField Unknown          = SQLInteger 0
  toField Image            = SQLInteger 1
  toField Webpage          = SQLInteger 2
  toField FlickrImage      = SQLInteger 3
  toField GigaPan          = SQLInteger 6
  toField Zoomify          = SQLInteger 7
  toField PDF10            = SQLInteger 10
  toField PDF11            = SQLInteger 11
  toField WebpageThumbnail = SQLInteger 14

instance FromField ContentType where
  fromField (Field (SQLInteger 0) _) = Ok Unknown
  fromField (Field (SQLInteger 1) _) = Ok Image
  fromField (Field (SQLInteger 2) _) = Ok Webpage
  fromField (Field (SQLInteger 3) _) = Ok FlickrImage
  fromField (Field (SQLInteger 6) _) = Ok GigaPan
  fromField (Field (SQLInteger 7) _) = Ok Zoomify
  fromField (Field (SQLInteger 10) _) = Ok PDF10
  fromField (Field (SQLInteger 11) _) = Ok PDF11
  fromField (Field (SQLInteger 14) _) = Ok WebpageThumbnail
  fromField f = returnError ConversionFailed f "invalid content type"
