module ZoomHub.API.Types.Callback
  ( Callback
  , unCallback
  ) where

import qualified Data.Text as T
import           Servant   (FromText, fromText)


newtype Callback = Callback { unCallback :: String } deriving (Eq, Show)

-- Text
-- TODO: Disallow invalid JavaScript identifiers:
instance FromText Callback where
  fromText = Just . Callback . T.unpack
