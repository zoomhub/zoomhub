module ZoomHub.Types.TempPath
  ( TempPath(TempPath)
  , unTempPath
  ) where

import           Data.Aeson (ToJSON, Value (String), toJSON)
import qualified Data.Text  as T

newtype TempPath = TempPath { unTempPath :: FilePath }
  deriving (Eq, Show)

-- JSON
instance ToJSON TempPath where
  toJSON = String . T.pack . unTempPath
