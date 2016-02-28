module ZoomHub.Types.BaseURI
  ( BaseURI(BaseURI)
  , unBaseURI
  ) where

import           Network.URI (URI)

newtype BaseURI = BaseURI { unBaseURI :: URI } deriving Eq

instance Show BaseURI where
  show = show . unBaseURI
