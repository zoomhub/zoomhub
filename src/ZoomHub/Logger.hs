-- Adapted from: https://git.io/v27pJ
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Logger (formatAsJSON) where

import qualified Blaze.ByteString.Builder             as BB
import           Data.Aeson                           (ToJSON, Value (String),
                                                       encode, object, toJSON,
                                                       (.=))
import qualified Data.ByteString.Char8                as S8
import           Data.CaseInsensitive                 (original)
import qualified Data.HashMap.Strict                  as HM
import           Data.IP                              (fromHostAddress,
                                                       fromIPv4)
import           Data.Monoid                          ((<>))
import           Data.Text                            (Text)
import qualified Data.Text                            as T
import           Data.Text.Encoding                   (decodeUtf8)
import           Data.Time                            (NominalDiffTime)
import           Data.Word                            (Word32)
import           Network.HTTP.Types                   as H
import           Network.Socket                       (PortNumber,
                                                       SockAddr (..))
import           Network.Wai                          (Request, RequestBodyLength (KnownLength, ChunkedBody),
                                                       httpVersion, queryString,
                                                       rawPathInfo, remoteHost,
                                                       requestBodyLength,
                                                       requestHeaders,
                                                       requestMethod)
import           Network.Wai.Middleware.RequestLogger (OutputFormatterWithDetails)
import           System.Log.FastLogger                (toLogStr)
import           Text.Printf                          (printf)

formatAsJSON :: OutputFormatterWithDetails
formatAsJSON date req status responseSize duration reqBody response =
  toLogStr (encode $
    object
      [ "req" .= requestToJSON duration req reqBody
      , "res" .=
      object
        [ "status" .= statusCode status
        , "size" .= responseSize
        , "body" .=
          if statusCode status >= 400
            then Just . decodeUtf8 . BB.toByteString $ response
            else Nothing
        ]
      , "time" .= decodeUtf8 date
      ]) <> "\n"

word32ToHostAddress :: Word32 -> Text
word32ToHostAddress =
  T.intercalate "." . map (T.pack . show) . fromIPv4 . fromHostAddress

readAsDouble :: String -> Double
readAsDouble = read

requestToJSON :: NominalDiffTime -> Request -> [S8.ByteString] -> Value
requestToJSON duration req reqBody =
  object
    [ "method" .= decodeUtf8 (requestMethod req)
    , "path" .= decodeUtf8 (rawPathInfo req)
    , "query" .= toObject (map queryItemToJSON (queryString req))
    , "duration" .= toMilliseconds duration
    , "size" .= requestBodyLengthToJSON (requestBodyLength req)
    , "body" .= decodeUtf8 (S8.concat reqBody)
    , "remoteHost" .= sockToJSON (remoteHost req)
    , "httpVersion" .= httpVersionToJSON (httpVersion req)
    , "headers" .= requestHeadersToJSON (requestHeaders req)
    ]
  where
    rationalToDouble :: Rational -> Double
    rationalToDouble = fromRational

    toMilliseconds :: NominalDiffTime -> Double
    toMilliseconds d = readAsDouble . printf "%.2f" . rationalToDouble $
      toRational d * 1000

sockToJSON :: SockAddr -> Value
sockToJSON (SockAddrInet pn ha) =
  object
    [ "port" .= portToJSON pn
    , "hostAddress" .= word32ToHostAddress ha
    ]
sockToJSON (SockAddrInet6 pn _ ha _) =
  object
    [ "port" .= portToJSON pn
    , "hostAddress" .= ha
    ]
sockToJSON (SockAddrUnix sock) =
  object [ "unix" .= sock ]
sockToJSON (SockAddrCan i) =
  object [ "can" .= i ]

toObject :: ToJSON a => [(Text, a)] -> Value
toObject = toJSON . HM.fromList

queryItemToJSON :: QueryItem -> (Text, Maybe Value)
queryItemToJSON (name, maybeValue) =
  (decodeUtf8 name, fmap (String . decodeUtf8) maybeValue)

requestHeadersToJSON :: RequestHeaders -> Value
requestHeadersToJSON = toObject . map hToJ where
  -- Redact cookies
  hToJ ("Cookie", _) = ("Cookie" :: Text, "<redacted>" :: Text)
  hToJ hd = headerToJSON hd

headerToJSON :: Header -> (Text, Text)
headerToJSON (headerName, header) =
  (decodeUtf8 . original $ headerName, decodeUtf8 header)

portToJSON :: PortNumber -> Value
portToJSON = toJSON . toInteger

httpVersionToJSON :: HttpVersion -> Value
httpVersionToJSON (HttpVersion major minor) =
  String $ T.pack (show major) <> "." <> T.pack (show minor)

requestBodyLengthToJSON :: RequestBodyLength -> Value
requestBodyLengthToJSON ChunkedBody = String "Unknown"
requestBodyLengthToJSON (KnownLength l) = toJSON l
