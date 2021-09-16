{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.URI.Instances where

import Network.URI (URI, parseURIReference)
import System.Envy (Var, fromVar, toVar)

instance Var URI where
  toVar = show

  fromVar = parseURIReference
