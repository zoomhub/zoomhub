{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.API.JSONP.Errors
  ( mkError
  )
  where

import           Data.Aeson                           (ToJSON)
import qualified Data.ByteString.UTF8            as BU
import qualified Data.ByteString.Lazy.UTF8            as BLU
import           Servant.Server                       (ServantErr (ServantErr),
                                                       errBody, errHTTPCode,
                                                       errHeaders,
                                                       errReasonPhrase)
import Network.HTTP.Types.Status (ok200, statusCode, statusMessage)

import           ZoomHub.API.ContentTypes.JavaScript  (toJS)
import           ZoomHub.API.Types.JSONP              (JSONP)
import           ZoomHub.API.Types.NonRESTfulResponse (NonRESTfulResponse)


-- HACK: JSONP errors need to be HTTP status 200 so clients are able to parse
-- them. Because their payload doesn’t match the regular response type, e.g.
-- `JSONP (NonRESTful Content)` for success vs `JSONP (NonRESTful String)` for
-- an error, we need to use `ServantErr` as an escape hatch, hence we create a
-- `ServantErr` with HTTP status 200:
mkError :: ToJSON a => JSONP (NonRESTfulResponse a) -> ServantErr
mkError body = ServantErr
    { errHTTPCode = statusCode status
    , errReasonPhrase = BU.toString (statusMessage status)
    -- TODO: Deduplicate using `JavaScript` content type:
    , errHeaders = [("Content-Type", "text/javascript; charset=utf-8")]
    , errBody = BLU.fromString (toJS body)
    }
  where status = ok200
