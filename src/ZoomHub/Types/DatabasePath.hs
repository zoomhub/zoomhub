module ZoomHub.Types.DatabasePath
  ( DatabasePath(DatabasePath)
  , unDatabasePath
  ) where

import           Data.Aeson (ToJSON, Value (String), toJSON)
import qualified Data.Text  as T

newtype DatabasePath = DatabasePath { unDatabasePath :: FilePath }
  deriving (Eq, Show)

-- JSON
instance ToJSON DatabasePath where
  toJSON = String . T.pack . unDatabasePath
