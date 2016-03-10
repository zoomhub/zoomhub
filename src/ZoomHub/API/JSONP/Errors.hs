{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.API.JSONP.Errors
  ( error400
  , error404
  , error503
  )
  where

import           Data.Aeson                           (ToJSON)
import qualified Data.ByteString.Lazy.UTF8            as BU
import           Servant                              (ServantErr, err400,
                                                       err404, err503, errBody,
                                                       errHeaders)

import           ZoomHub.API.ContentTypes.JavaScript  (toJS)
import           ZoomHub.API.Types.JSONP              (JSONP)
import           ZoomHub.API.Types.NonRESTfulResponse (NonRESTfulResponse)


error400 :: ToJSON a => JSONP (NonRESTfulResponse a) -> ServantErr
error400 = mkError err400

error404 :: ToJSON a => JSONP (NonRESTfulResponse a) -> ServantErr
error404 = mkError err404

error503 :: ToJSON a => JSONP (NonRESTfulResponse a) -> ServantErr
error503 = mkError err503

mkError :: ToJSON a => ServantErr -> JSONP (NonRESTfulResponse a) -> ServantErr
mkError errorType body = errorType
  { errHeaders = [("Content-Type", "text/javascript; charset=utf-8")]
  , errBody = BU.fromString (toJS body)
  }
