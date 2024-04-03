module ZoomHub.Utils
  ( intercalate,
    lenientDecodeUtf8,
    tshow,
    uriByteStringToURI,
  )
where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import Data.List (intersperse)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Flow
import Network.URI (URI, parseAbsoluteURI)
import URI.ByteString (serializeURIRef')
import qualified URI.ByteString as BS

intercalate :: (Monoid a) => a -> [a] -> a
intercalate xs xss = mconcat (intersperse xs xss)

lenientDecodeUtf8 :: ByteString -> Text
lenientDecodeUtf8 = decodeUtf8With lenientDecode

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

uriByteStringToURI :: BS.URI -> Maybe URI
uriByteStringToURI uriBS =
  uriBS |> serializeURIRef' |> BC.unpack |> parseAbsoluteURI
