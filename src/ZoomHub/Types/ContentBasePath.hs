module ZoomHub.Types.ContentBasePath
  ( ContentBasePath(ContentBasePath)
  , unContentBasePath
  ) where

import           Data.Aeson (ToJSON, Value (String), toJSON)
import           Data.Text  (Text)
import qualified Data.Text  as T

newtype ContentBasePath = ContentBasePath { unContentBasePath :: Text }
  deriving Eq

instance Show ContentBasePath where
  show = T.unpack . unContentBasePath

instance ToJSON ContentBasePath where
  toJSON = String . unContentBasePath
