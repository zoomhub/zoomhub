{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.URI.Instances where

import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T
import Network.URI (URI, parseURIReference)
import Servant (ToHttpApiData (toHeader, toUrlPiece))
import System.Envy (Var, fromVar, toVar)

instance Var URI where
  toVar = show

  fromVar = parseURIReference

instance ToHttpApiData URI where
  toUrlPiece = T.pack . show
  toHeader = BC.pack . show
