module ZoomHub.Utils
  ((<$$>)
  , intercalate
  ) where

import           Data.List (intersperse)

-- Operators:
-- See: https://mail.haskell.org/pipermail/libraries/2010-April/013417.html
(<$$>) :: (Functor f) => f a -> (a -> b) -> f b
(<$$>) = flip (<$>)

intercalate :: Monoid a => a -> [a] -> a
intercalate xs xss = mconcat (intersperse xs xss)
