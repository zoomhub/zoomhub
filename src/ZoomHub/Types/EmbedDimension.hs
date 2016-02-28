module ZoomHub.Types.EmbedDimension
  ( EmbedDimension(..)
  , toCSSValue
  ) where

-- Type
data EmbedDimension = Zero | Auto | Pixels Integer | Percentage Integer
  deriving (Eq, Show)

toCSSValue :: EmbedDimension -> String
toCSSValue Auto = "auto"
toCSSValue Zero = "0"
toCSSValue (Pixels n) = show n ++ "px"
toCSSValue (Percentage n) = show n ++ "%"
