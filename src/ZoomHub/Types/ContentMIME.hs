module ZoomHub.Types.ContentMIME
  ( ContentMIME(ContentMIME)
  , unContentMIME
  ) where

import           Codec.MIME.Parse                 (parseMIMEType)
import           Codec.MIME.Type                  (Type, showType)
import           Data.Aeson                       (ToJSON, Value (String),
                                                   toJSON)
import qualified Data.Text                        as T
import           Database.SQLite.Simple           (SQLData (SQLText))
import           Database.SQLite.Simple.FromField (FromField, ResultError (ConversionFailed),
                                                   fromField, returnError)
import           Database.SQLite.Simple.Internal  (Field (Field))
import           Database.SQLite.Simple.Ok        (Ok (Ok))
import           Database.SQLite.Simple.ToField   (ToField, toField)

newtype ContentMIME = ContentMIME
  { unContentMIME :: Type
  } deriving (Eq, Show)

-- SQLite
instance ToField ContentMIME where
  toField = SQLText . showType . unContentMIME

instance FromField ContentMIME where
  fromField f@(Field (SQLText t) _) =
    case parseMIMEType t of
      Just r -> Ok (ContentMIME r)
      Nothing ->
        returnError
          ConversionFailed
          f
          ("couldn’t parse MIME type: " ++ T.unpack t)
  fromField f = returnError ConversionFailed f "invalid MIME type"

-- JSON
instance ToJSON ContentMIME where
  toJSON = String . showType . unContentMIME
