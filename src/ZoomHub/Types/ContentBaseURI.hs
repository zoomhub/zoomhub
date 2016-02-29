module ZoomHub.Types.ContentBaseURI
  ( ContentBaseURI(ContentBaseURI)
  , unContentBaseURI
  ) where

import           Network.URI (URI)

newtype ContentBaseURI = ContentBaseURI { unContentBaseURI :: URI } deriving Eq

instance Show ContentBaseURI where
  show = show . unContentBaseURI
