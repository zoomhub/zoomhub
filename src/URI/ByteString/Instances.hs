{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module URI.ByteString.Instances where

import Data.Text.Encoding (decodeUtf8Lenient)
import Servant (ToHttpApiData (toHeader, toUrlPiece))
import URI.ByteString (URI, serializeURIRef')

instance ToHttpApiData URI where
  toUrlPiece = decodeUtf8Lenient . serializeURIRef'
  toHeader = serializeURIRef'
