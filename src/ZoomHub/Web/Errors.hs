{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Web.Errors
  ( error400,
    error401,
    error404,
    error503,
  )
where

import qualified Data.ByteString.Lazy.UTF8 as BU
import Servant (ServerError, err400, err401, err404, err503, errBody, errHeaders)

error400 :: String -> ServerError
error400 = mkError err400

error401 :: String -> ServerError
error401 = mkError err401

error404 :: String -> ServerError
error404 = mkError err404

error503 :: String -> ServerError
error503 = mkError err503

mkError :: ServerError -> String -> ServerError
mkError errorType message =
  errorType
    { errHeaders = [("Content-Type", "text/html; charset=utf-8")],
      errBody = BU.fromString message
    }
