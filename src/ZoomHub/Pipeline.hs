module ZoomHub.Pipeline
  ( process
  )
  where

import           Control.Lens                             ((^.))
import           Network.Wreq                             (get, responseBody)
import           System.AtomicWrite.Writer.LazyByteString (atomicWriteFile)
import           System.Directory                         (createDirectoryIfMissing)
import           System.FilePath.Posix                    ((<.>), (</>))
-- import           System.IO.Temp
import           System.Process                           (callProcess)

import           ZoomHub.Config                           (Config)
import qualified ZoomHub.Config                           as Config
import           ZoomHub.Types.Content                    (Content, contentId,
                                                           contentUrl)
import           ZoomHub.Types.ContentId                  (unId)
import           ZoomHub.Types.DeepZoomImage              (TileOverlap (TileOverlap1), TileSize (TileSize254))


process :: Config -> Content -> IO Content
process config content = do
  -- withTempDirectory tempPath template $ \tmpDir -> do
    -- let rawPath = tmpDir </> rawContentId
    let rawPath = tempPath </> rawContentId
    createDirectoryIfMissing False tempPath
    putStrLn $ "Downloading: " ++ show (contentUrl content)
    downloadURL (show (contentUrl content)) rawPath
    putStrLn $ "Creating DZI: " ++ rawContentId
    createDZI rawPath (rawPath <.> ".dzi")
    return content
  where
    tempPath = Config.dataPath config </> "content-raw"
    rawContentId = unId $ contentId content
    -- template = rawContentId ++ "."

downloadURL :: String -> FilePath -> IO ()
downloadURL url dest = do
  resp <- get url
  let body = resp ^. responseBody
  atomicWriteFile dest body

-- createDZI ::  FilePath -> FilePath -> IO DeepZoomImage
createDZI :: FilePath -> FilePath -> IO ()
createDZI src dest =
  callProcess "vips"
    [ "dzsave"
    , "--tile-size", show TileSize254
    , "--overlap", show TileOverlap1
    , src
    , dest
    , "--vips-progress"
    ]
