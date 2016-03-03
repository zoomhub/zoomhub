{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.Web.Errors
  ( error400
  , error404
  )
  where

import qualified Data.ByteString.Lazy.UTF8 as BU
import           Servant                   (ServantErr, err400, err404, errBody,
                                            errHeaders)

error400 :: String -> ServantErr
error400 = mkError err400

error404 :: String -> ServantErr
error404 = mkError err404

mkError :: ServantErr -> String -> ServantErr
mkError errorType message = errorType
  { errHeaders = [("Content-Type", "text/html; charset=utf-8")]
  , errBody = BU.fromString message
  }
