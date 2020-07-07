module ZoomHub.Types.BaseURI
  ( BaseURI (BaseURI),
    unBaseURI,
  )
where

import Data.Aeson (ToJSON, Value (String), toJSON)
import qualified Data.Text as T
import Network.URI (URI)

newtype BaseURI = BaseURI {unBaseURI :: URI} deriving (Eq)

instance Show BaseURI where
  show = show . unBaseURI

instance ToJSON BaseURI where
  toJSON = String . T.pack . show
