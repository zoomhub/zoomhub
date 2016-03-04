{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Web.Static
  ( serveDirectory
  ) where


import qualified Data.ByteString.Lazy           as BL
import           Network.HTTP.Types             (status404)
import           Network.Wai                    (Application, responseLBS)
import           Network.Wai.Application.Static (defaultFileServerSettings,
                                                 ss404Handler, staticApp)
import           Servant.API.Raw                (Raw)
import           Servant.Server                 (Server)
import           System.FilePath                (addTrailingPathSeparator)

serveDirectory :: BL.ByteString -> FilePath -> Server Raw
serveDirectory error404 root =
    staticApp (defaultFileServerSettings normalizedRoot) {
      ss404Handler = Just (custom404Handler error404)
    }
  where normalizedRoot = addTrailingPathSeparator root

custom404Handler :: BL.ByteString -> Application
custom404Handler body _ sendResponse =
    sendResponse $ responseLBS status404 headers body
  where headers = [("Content-Type", "text/html; charset=utf-8")]
