{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module URI.ByteString.Instances where

import Servant (ToHttpApiData (toHeader, toUrlPiece))
import URI.ByteString (URI, serializeURIRef')
import ZoomHub.Utils (lenientDecodeUtf8)

instance ToHttpApiData URI where
  toUrlPiece = lenientDecodeUtf8 . serializeURIRef'
  toHeader = serializeURIRef'
