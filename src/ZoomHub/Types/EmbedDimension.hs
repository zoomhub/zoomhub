module ZoomHub.Types.EmbedDimension
  ( EmbedDimension(..)
  , toCSSValue
  , parseCSSValue
  ) where

import           Data.Char (isDigit)
import qualified Data.Text as T
import           Safe      (readMay)
import           Servant   (FromText, fromText)

-- Type
data EmbedDimension = Zero | Auto | Pixels Integer | Percentage Integer
  deriving (Eq, Show)

toCSSValue :: EmbedDimension -> String
toCSSValue Auto = "auto"
toCSSValue Zero = "0"
toCSSValue (Pixels n) = show n ++ "px"
toCSSValue (Percentage n) = show n ++ "%"

parseCSSValue :: String -> Maybe EmbedDimension
parseCSSValue "auto" = Just Auto
parseCSSValue "0" = Just Zero
parseCSSValue s = case dimension of
    "px" -> readMay prefix >>= Just . Pixels
    "%"  -> readMay prefix >>= Just . Percentage
    _    -> Nothing
  where
    dimension = dropWhile isDigit s
    prefix = takeWhile isDigit s

-- Text
instance FromText EmbedDimension where
  fromText = parseCSSValue . T.unpack
