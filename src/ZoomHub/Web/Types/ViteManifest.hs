{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Web.Types.ViteManifest
  ( AssetPath (..),
    readAssetPath,
  )
where

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KeyMap
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import System.FilePath ((</>))

-- | A resolved asset path from Vite's build manifest, e.g.
-- @\/assets\/global-abc123.css@.
newtype AssetPath = AssetPath {unAssetPath :: Text}
  deriving (Eq, Show)

-- | Look up a source path in Vite's build manifest and return the hashed
-- output path. Vite generates a manifest at @.vite/manifest.json@ mapping
-- source entry points to their build outputs:
--
-- @
-- { "src/styles/global.css": { "file": "assets/global-abc123.css", ... } }
-- @
readAssetPath :: FilePath -> Text -> IO AssetPath
readAssetPath publicPath srcPath = do
  let manifestPath = publicPath </> ".vite" </> "manifest.json"
  bytes <- BL.readFile manifestPath
  case lookupAssetPath srcPath bytes of
    Just path -> pure $ AssetPath $ "/" <> path
    Nothing ->
      error $
        "ZoomHub.Web.Types.ViteManifest: Could not find '"
          <> show srcPath
          <> "' in "
          <> manifestPath

lookupAssetPath :: Text -> BL.ByteString -> Maybe Text
lookupAssetPath srcPath bytes = do
  Aeson.Object manifest <- Aeson.decode bytes
  Aeson.Object entry <- KeyMap.lookup (Key.fromText srcPath) manifest
  Aeson.String file <- KeyMap.lookup "file" entry
  pure file
