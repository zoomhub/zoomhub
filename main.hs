import Network.Wai
import Network.Wai.Handler.Warp


import qualified ZoomHub

main :: IO ()
main = run 8000 ZoomHub.app
