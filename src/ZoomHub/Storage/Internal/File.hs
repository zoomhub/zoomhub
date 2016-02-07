module ZoomHub.Storage.Internal.File where

import           Crypto.Hash           (Digest, SHA256, hash)
import qualified Data.ByteString.Char8 as BC
import           Data.Char             (isUpper)


toFilename :: String -> Maybe FilePath
toFilename "" = Just ""
toFilename (c:cs)
  | c == '_'  = Nothing
  | isUpper c = (\x -> '_' : c : x) <$> toFilename cs
  | otherwise = (\x -> c : x) <$> toFilename cs

toId :: FilePath -> String
toId "" = ""
toId (c:cs)
  | c == '_'  = toId cs
  | otherwise = c : toId cs

hashURL :: String -> String
hashURL url = show (hash $ BC.pack url :: Digest SHA256)
