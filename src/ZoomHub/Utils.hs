module ZoomHub.Utils
  ( intercalate,
    lenientDecodeUtf8,
    tshow,
    hush,
    appendQueryParams,
  )
where

import Control.Lens (over)
import Data.ByteString (ByteString)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8Lenient)
import URI.ByteString (URIRef, queryL, queryPairsL)

intercalate :: (Monoid a) => a -> [a] -> a
intercalate xs xss = mconcat (intersperse xs xss)

lenientDecodeUtf8 :: ByteString -> Text
lenientDecodeUtf8 = decodeUtf8Lenient

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

hush :: Either a b -> Maybe b
hush = either (const Nothing) Just

appendQueryParams :: [(ByteString, ByteString)] -> URIRef a -> URIRef a
appendQueryParams params =
  over (queryL . queryPairsL) (params <>)
