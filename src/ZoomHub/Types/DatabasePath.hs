module ZoomHub.Types.DatabasePath
  ( DatabasePath(DatabasePath)
  , unDatabasePath
  ) where

newtype DatabasePath = DatabasePath { unDatabasePath :: FilePath }
  deriving (Eq, Show)
