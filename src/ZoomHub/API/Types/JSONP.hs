{-# LANGUAGE ExistentialQuantification #-}

module ZoomHub.API.Types.JSONP
  ( JSONP,
    jsonpBody,
    jsonpCallback,
    mkJSONP,
  )
where

import Data.Aeson (ToJSON)
import ZoomHub.API.Types.Callback

data JSONP a = ToJSON a =>
  JSONP
  { jsonpBody :: a,
    jsonpCallback :: Callback
  }

mkJSONP :: ToJSON a => Callback -> a -> JSONP a
mkJSONP callback body =
  JSONP
    { jsonpBody = body,
      jsonpCallback = callback
    }
