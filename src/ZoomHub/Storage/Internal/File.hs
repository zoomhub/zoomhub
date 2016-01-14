module ZoomHub.Storage.Internal.File where

import Data.Char (isUpper)

toFilename :: String -> FilePath
toFilename "" = ""
toFilename (c:cs)
  | c == '_' = error "toFilename: Underscore (`_`) is not allowed in IDs."
  | isUpper c = '_' : c : toFilename cs
  | otherwise = c : toFilename cs

toId :: FilePath -> String
toId "" = ""
toId (c:cs)
  | c == '_'  = toId cs
  | otherwise = c : toId cs
