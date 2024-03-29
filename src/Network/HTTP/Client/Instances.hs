{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.HTTP.Client.Instances where

import Data.Aeson (ToJSON, Value (String), object, toJSON, (.=))
import qualified Data.ByteString as BS
import Data.CaseInsensitive (original)
import qualified Data.HashMap.Strict as HM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Network.HTTP.Client
  ( HttpException (HttpExceptionRequest, InvalidUrlException),
    HttpExceptionContent (ConnectionFailure, StatusCodeException),
    host,
    method,
    path,
    port,
    queryString,
    responseHeaders,
    responseStatus,
  )
import Network.HTTP.Types (Header, ResponseHeaders)
import Network.HTTP.Types.Status (statusCode)

-- Number of bytes we store for responses with exceptions:
maxBodyBytes :: Int
maxBodyBytes = 256

instance ToJSON HttpException where
  toJSON (InvalidUrlException url reason) =
    object
      [ "type" .= ("InvalidUrlException" :: Text),
        "url" .= url,
        "reason" .= reason
      ]
  toJSON (HttpExceptionRequest r (ConnectionFailure e)) =
    object
      [ "type" .= ("ConnectionFailure" :: Text),
        "host" .= lenientDecodeUtf8 (host r),
        "method" .= show (method r),
        "port" .= port r,
        "path" .= lenientDecodeUtf8 (path r),
        "query" .= lenientDecodeUtf8 (queryString r),
        "exception" .= toJSONString (show e)
      ]
  toJSON (HttpExceptionRequest _ (StatusCodeException res _)) =
    object
      [ "type" .= ("StatusCodeException" :: Text),
        "status" .= statusCode (responseStatus res),
        "headers" .= headersToJSON (responseHeaders res)
      ]
  toJSON e = String . T.pack . show $ e

toJSONString :: String -> Value
toJSONString = String . T.pack

-- Duplicated from `RequestLogger`:
toObject :: (ToJSON a) => [(Text, a)] -> Value
toObject = toJSON . HM.fromList

headersToJSON :: ResponseHeaders -> Value
headersToJSON = toObject . map headerToJSON'
  where
    headerToJSON' ("Cookie", _) = ("Cookie" :: Text, "<redacted>" :: Text)
    headerToJSON' ("X-Response-Body-Start", v) =
      ( "X-Response-Body-Start" :: Text,
        lenientDecodeUtf8 $ BS.take maxBodyBytes v
      )
    headerToJSON' hd = headerToJSON hd
    headerToJSON :: Header -> (Text, Text)
    headerToJSON (headerName, header) =
      (lenientDecodeUtf8 . original $ headerName, lenientDecodeUtf8 header)

lenientDecodeUtf8 :: BS.ByteString -> Text
lenientDecodeUtf8 = decodeUtf8With lenientDecode
