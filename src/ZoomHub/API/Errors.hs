{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.API.Errors
  ( error400
  , error404
  )
  where

import           Data.Aeson (encode, object, (.=))
import qualified Data.Text  as T
import           Servant    (ServantErr, err400, err404, errBody, errHeaders)

error400 :: String -> ServantErr
error400 = mkError err400

error404 :: String -> ServantErr
error404 = mkError err404

mkError :: ServantErr -> String -> ServantErr
mkError errorType message = errorType
  { errHeaders = [("Content-Type", "application/json")]
  , errBody = encode $ object ["error" .= object ["message" .= T.pack message]]
  }

