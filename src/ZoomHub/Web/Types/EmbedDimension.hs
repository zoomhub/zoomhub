module ZoomHub.Web.Types.EmbedDimension
  ( EmbedDimension(..)
  , toCSSValue
  , parseCSSValue
  ) where

import           Data.Bifunctor (first)
import           Data.Char      (isDigit)
import qualified Data.Text      as T
import           Safe           (readEitherSafe)
import           Servant        (FromHttpApiData, parseUrlPiece)

-- Type
data EmbedDimension = Zero
                    | Auto
                    | Pixels Integer
                    | Percentage Integer
                    deriving (Eq, Show)

toCSSValue :: EmbedDimension -> String
toCSSValue Auto = "auto"
toCSSValue Zero = "0"
toCSSValue (Pixels n) = show n ++ "px"
toCSSValue (Percentage n) = show n ++ "%"

parseCSSValue :: String -> Either String EmbedDimension
parseCSSValue "auto"        = Right Auto
parseCSSValue "0"           = Right Zero
parseCSSValue s = case unit of
    "px" -> readEitherSafe value >>= Right . Pixels
    "%"  -> readEitherSafe value >>= Right . Percentage
    u    -> Left $ "Invalid CSS unit: " <> u
  where
    value = takeWhile isDigit s
    unit = dropWhile isDigit s

-- Text
instance FromHttpApiData EmbedDimension where
  parseUrlPiece p = first T.pack $ parseCSSValue . T.unpack $ p
