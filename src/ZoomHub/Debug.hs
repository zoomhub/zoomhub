module ZoomHub.Debug
  ( spy,
  )
where

import qualified Debug.Trace as Trace

spy :: (Show a) => String -> a -> a
spy tag x = Trace.trace (tag <> ": " <> show x) x
