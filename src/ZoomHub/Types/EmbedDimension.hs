module ZoomHub.Types.EmbedDimension
  ( EmbedDimension(..)
  , toCSSValue
  , parseCSSValue
  ) where

import qualified Data.Text as T
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
-- TODO: Add support for parsing `Pixels` and `Percentage`:
parseCSSValue _ = Nothing

-- Text
instance FromText EmbedDimension where
  fromText = parseCSSValue . T.unpack
