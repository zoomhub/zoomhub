module ZoomHub.Types.ContentBaseURI
  ( ContentBaseURI(ContentBaseURI)
  , unContentBaseURI
  ) where

import           Data.Aeson  (ToJSON, Value (String), toJSON)
import qualified Data.Text   as T
import           Network.URI (URI)

newtype ContentBaseURI = ContentBaseURI { unContentBaseURI :: URI } deriving Eq

instance Show ContentBaseURI where
  show = show . unContentBaseURI

instance ToJSON ContentBaseURI where
  toJSON = String . T.pack . show
