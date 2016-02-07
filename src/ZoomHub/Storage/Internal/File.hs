module ZoomHub.Storage.Internal.File where

import           Crypto.Hash           (Digest, SHA256, hash)
import qualified Data.ByteString.Char8 as BC
import           Data.Char             (isUpper)


-- TODO: Make this a total function using `Maybe FilePath` return value:
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

hashURL :: String -> String
hashURL url = show (hash $ BC.pack url :: Digest SHA256)
