module ZoomHub.Utils
  ( intercalate,
    tshow,
    hush,
    appendQueryParams,
  )
where

import Control.Lens (over)
import Data.ByteString (ByteString)
import Data.List (intersperse)
import qualified Data.Text as T
import URI.ByteString (URIRef, queryL, queryPairsL)

intercalate :: (Monoid a) => a -> [a] -> a
intercalate xs xss = mconcat (intersperse xs xss)

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

appendQueryParams :: [(ByteString, ByteString)] -> URIRef a -> URIRef a
appendQueryParams params =
  over (queryL . queryPairsL) (params <>)
