module ZoomHub.API.Types.Callback
  ( Callback
  , unCallback
  ) where

import qualified Data.Text as T
import           Servant   (FromHttpApiData (..))

newtype Callback = Callback
  { unCallback :: String
  } deriving (Eq, Show)

-- TODO: Disallow invalid JavaScript identifiers:
instance FromHttpApiData Callback where
  parseUrlPiece = Right . Callback . T.unpack
