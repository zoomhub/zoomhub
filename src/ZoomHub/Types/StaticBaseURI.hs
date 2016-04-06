module ZoomHub.Types.StaticBaseURI
  ( StaticBaseURI(StaticBaseURI)
  , unStaticBaseURI
  ) where

import           Network.URI (URI)

newtype StaticBaseURI = StaticBaseURI { unStaticBaseURI :: URI } deriving Eq

instance Show StaticBaseURI where
  show = show . unStaticBaseURI
