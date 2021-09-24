module ZoomHub.Utils
  ( intercalate,
    lenientDecodeUtf8,
  )
where

import Data.ByteString (ByteString)
import Data.List (intersperse)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)

intercalate :: Monoid a => a -> [a] -> a
intercalate xs xss = mconcat (intersperse xs xss)

lenientDecodeUtf8 :: ByteString -> Text
lenientDecodeUtf8 = decodeUtf8With lenientDecode
