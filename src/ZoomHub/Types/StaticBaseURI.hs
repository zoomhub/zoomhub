module ZoomHub.Types.StaticBaseURI
  ( StaticBaseURI(StaticBaseURI)
  , unStaticBaseURI
  ) where

import           Data.Aeson  (ToJSON, Value (String), toJSON)
import qualified Data.Text   as T
import           Network.URI (URI)

newtype StaticBaseURI = StaticBaseURI { unStaticBaseURI :: URI } deriving Eq

instance Show StaticBaseURI where
  show = show . unStaticBaseURI

instance ToJSON StaticBaseURI where
  toJSON = String . T.pack . show
