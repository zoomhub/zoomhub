module ZoomHub.Config.Uploads
  ( Uploads (..),
    parse,
  )
where

import Data.Aeson (ToJSON, Value (String), toJSON)
import qualified Data.Text as T

data Uploads
  = UploadsDisabled
  | UploadsEnabled
  deriving (Eq, Show)

parse :: String -> Uploads
parse "1" = UploadsEnabled
parse "true" = UploadsEnabled
parse _ = UploadsDisabled

instance ToJSON Uploads where
  toJSON = String . T.pack . show
