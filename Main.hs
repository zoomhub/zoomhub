import qualified Network.Wai.Handler.Warp as Warp


import qualified ZoomHub.API as ZoomHub

main :: IO ()
main = Warp.run 8000 ZoomHub.app
