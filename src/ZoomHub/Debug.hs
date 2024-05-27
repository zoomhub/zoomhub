module ZoomHub.Debug
  ( spy,
    spyWith,
  )
where

import qualified Debug.Trace as Trace

spy :: (Show a) => String -> a -> a
spy tag = spyWith tag show

spyWith :: String -> (a -> String) -> a -> a
spyWith tag toString x = Trace.trace (tag <> ": " <> toString x) x
