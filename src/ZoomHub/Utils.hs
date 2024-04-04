module ZoomHub.Utils
  ( intercalate,
    lenientDecodeUtf8,
    tshow,
    appendQueryParams,
  )
where

import Control.Lens (over)
import Data.ByteString (ByteString)
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import URI.ByteString (URIRef, queryL, queryPairsL)

intercalate :: (Monoid a) => a -> [a] -> a
intercalate xs xss = mconcat (intersperse xs xss)

lenientDecodeUtf8 :: ByteString -> Text
lenientDecodeUtf8 = decodeUtf8With lenientDecode

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

appendQueryParams :: [(ByteString, ByteString)] -> URIRef a -> URIRef a
appendQueryParams params =
  over (queryL . queryPairsL) (params <>)
