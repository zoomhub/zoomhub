import qualified Network.Wai.Handler.Warp as Warp

import qualified System.Environment as System
import qualified ZoomHub.API as ZoomHub


main :: IO ()
main = do
  maybePort <- System.lookupEnv "PORT"
  case maybePort of
    Nothing   -> fail "Please specify a `PORT`"
    Just port -> Warp.run (read port) ZoomHub.app
