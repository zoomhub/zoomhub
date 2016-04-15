{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.HTTP.Client.Instances where

import           Data.Aeson                (ToJSON, Value (String), object,
                                            toJSON, (.=))
import qualified Data.ByteString           as BL
import           Data.CaseInsensitive      (original)
import qualified Data.HashMap.Strict       as HM
import           Data.Text                 (Text)
import qualified Data.Text                 as T
import           Data.Text.Encoding        (decodeUtf8)
import           Network.HTTP.Client       (HttpException (StatusCodeException, InternalIOException, FailedConnectionException, FailedConnectionException2, NoResponseDataReceived, ResponseTimeout))
import           Network.HTTP.Types        (Header, ResponseHeaders)
import           Network.HTTP.Types.Status (statusCode)

-- Number of bytes we store for responses with exceptions:
maxBodyBytes :: Int
maxBodyBytes = 256

instance ToJSON HttpException where
  toJSON (FailedConnectionException host port) = object
    [ "type" .= ("FailedConnectionException" :: Text)
    , "host" .= host
    , "port" .= port
    ]
  toJSON (FailedConnectionException2 host port secure cause) = object
    [ "type" .= ("FailedConnectionException2" :: Text)
    , "host" .= host
    , "port" .= port
    , "secure" .= secure
    , "cause" .= T.pack (show cause)
    ]
  toJSON (InternalIOException cause) = object
    [ "type" .= ("InternalIOException" :: Text)
    , "cause" .= T.pack (show cause)
    ]
  toJSON NoResponseDataReceived = object
    [ "type" .= ("NoResponseDataReceived" :: Text) ]
  toJSON ResponseTimeout = object
    [ "type" .= ("ResponseTimeout" :: Text) ]
  toJSON (StatusCodeException status headers _) = object
    [ "type" .= ("StatusCodeException" :: Text)
    , "status" .= statusCode status
    , "headers" .= headersToJSON headers
    ]
  toJSON e = String . T.pack . show $ e

-- Duplicated from `RequestLogger`:
toObject :: ToJSON a => [(Text, a)] -> Value
toObject = toJSON . HM.fromList

headersToJSON :: ResponseHeaders -> Value
headersToJSON = toObject . map headerToJSON'
  where
    headerToJSON' ("Cookie", _) = ("Cookie" :: Text, "<redacted>" :: Text)
    headerToJSON' ("X-Response-Body-Start", v) =
      ("X-Response-Body-Start" :: Text, decodeUtf8 (BL.take maxBodyBytes v))
    headerToJSON' hd = headerToJSON hd

    headerToJSON :: Header -> (Text, Text)
    headerToJSON (headerName, header) =
      (decodeUtf8 . original $ headerName, decodeUtf8 header)
