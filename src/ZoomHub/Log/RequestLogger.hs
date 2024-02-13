{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-deprecations #-}

-- Adapted from: https://git.io/v27pJ

module ZoomHub.Log.RequestLogger
  ( formatAsJSON,
  )
where

import qualified Blaze.ByteString.Builder as BB
import Data.Aeson (ToJSON, Value (String), object, toJSON, (.=))
import qualified Data.ByteString.Char8 as BC
import Data.CaseInsensitive (original)
import qualified Data.HashMap.Strict as HM
import Data.IP (fromHostAddress, fromIPv4)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime)
import Data.Time.Units (Millisecond)
import Data.Time.Units.Instances ()
import Data.Word (Word32)
import Network.HTTP.Types as H
import Network.Socket (PortNumber, SockAddr (..))
import Network.Wai
  ( Request,
    RequestBodyLength (ChunkedBody, KnownLength),
    httpVersion,
    queryString,
    rawPathInfo,
    remoteHost,
    requestBodyLength,
    requestHeaders,
    requestMethod,
  )
import Network.Wai.Middleware.RequestLogger (OutputFormatterWithDetails)
import System.Log.FastLogger (toLogStr)
import Text.Printf (printf)
import ZoomHub.Log.Logger (encodeLogLine)
import ZoomHub.Utils (lenientDecodeUtf8)

formatAsJSON :: OutputFormatterWithDetails
formatAsJSON date req status responseSize duration reqBody response =
  toLogStr
    ( encodeLogLine $
        object
          [ "type" .= ("access" :: Text),
            "req" .= requestToJSON duration req reqBody,
            "res"
              .= object
                [ "status" .= statusCode status,
                  "size" .= responseSize,
                  "body"
                    .= if statusCode status >= 400
                      then Just . lenientDecodeUtf8 . BB.toByteString $ response
                      else Nothing
                ],
            "time" .= lenientDecodeUtf8 date
          ]
    )
    <> "\n"

word32ToHostAddress :: Word32 -> Text
word32ToHostAddress =
  T.intercalate "." . map (T.pack . show) . fromIPv4 . fromHostAddress

readAsDouble :: String -> Double
readAsDouble = read

requestToJSON :: NominalDiffTime -> Request -> [BC.ByteString] -> Value
requestToJSON duration req reqBody =
  object
    [ "method" .= lenientDecodeUtf8 (requestMethod req),
      "path" .= lenientDecodeUtf8 (rawPathInfo req),
      "query" .= toObject (map queryItemToJSON (queryString req)),
      "duration" .= (round (toMilliseconds duration) :: Millisecond),
      "size" .= requestBodyLengthToJSON (requestBodyLength req),
      "body" .= (lenientDecodeUtf8 . BC.take 512 $ BC.concat reqBody),
      "remoteHost" .= sockToJSON (remoteHost req),
      "httpVersion" .= httpVersionToJSON (httpVersion req),
      "headers" .= requestHeadersToJSON (requestHeaders req)
    ]
  where
    rationalToDouble :: Rational -> Double
    rationalToDouble = fromRational
    toMilliseconds :: NominalDiffTime -> Double
    toMilliseconds d =
      readAsDouble . printf "%.2f" . rationalToDouble $
        toRational d * 1000

sockToJSON :: SockAddr -> Value
sockToJSON (SockAddrInet pn ha) =
  object
    [ "port" .= portToJSON pn,
      "hostAddress" .= word32ToHostAddress ha
    ]
sockToJSON (SockAddrInet6 pn _ ha _) =
  object
    [ "port" .= portToJSON pn,
      "hostAddress" .= ha
    ]
sockToJSON (SockAddrUnix sock) =
  object ["unix" .= sock]
sockToJSON (SockAddrCan i) =
  object ["can" .= i]

toObject :: ToJSON a => [(Text, a)] -> Value
toObject = toJSON . HM.fromList

queryItemToJSON :: QueryItem -> (Text, Maybe Value)
queryItemToJSON (name, maybeValue) =
  (lenientDecodeUtf8 name, fmap (String . lenientDecodeUtf8) maybeValue)

requestHeadersToJSON :: RequestHeaders -> Value
requestHeadersToJSON = toObject . map hToJ
  where
    -- Redact cookies
    hToJ ("Cookie", _) = ("Cookie" :: Text, "<redacted>" :: Text)
    hToJ hd = headerToJSON hd

headerToJSON :: Header -> (Text, Text)
headerToJSON (headerName, header) =
  ( lenientDecodeUtf8 . original $ headerName,
    lenientDecodeUtf8 header
  )

portToJSON :: PortNumber -> Value
portToJSON = toJSON . toInteger

httpVersionToJSON :: HttpVersion -> Value
httpVersionToJSON (HttpVersion major minor) =
  String $ T.pack (show major) <> "." <> T.pack (show minor)

requestBodyLengthToJSON :: RequestBodyLength -> Value
requestBodyLengthToJSON ChunkedBody = String "Unknown"
requestBodyLengthToJSON (KnownLength l) = toJSON l
