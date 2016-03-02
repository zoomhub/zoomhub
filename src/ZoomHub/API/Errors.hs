{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.API.Errors
  ( error400
  , error404
  )
  where

import qualified Data.ByteString.Lazy.Char8 as BLC
import           Servant                    (ServantErr, err400, err404,
                                             errBody, errHeaders)

error400 :: String -> ServantErr
error400 = mkError err400

error404 :: String -> ServantErr
error404 = mkError err404

mkError :: ServantErr -> String -> ServantErr
mkError errorType message = errorType
  { errHeaders = [("Content-Type", "text/plain")]
  , errBody = BLC.pack message
  }
