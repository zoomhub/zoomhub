module ZoomHub.Utils
  ((<$$>)
  ) where

-- Operators:
-- See: https://mail.haskell.org/pipermail/libraries/2010-April/013417.html
(<$$>) :: (Functor f) => f a -> (a -> b) -> f b
(<$$>) = flip (<$>)
