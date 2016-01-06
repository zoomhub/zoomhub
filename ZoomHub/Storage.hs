module ZoomHub.Storage
  ( toFilename
  )
  where

import qualified Data.Char as Char


toFilename :: String -> FilePath
toFilename [] = []
toFilename (c:cs)
  | Char.isUpper c = '_' : c : toFilename cs
  | otherwise = c : toFilename cs
