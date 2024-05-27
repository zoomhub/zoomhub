{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module ZoomHub.API.Types.NonRESTfulResponse
  ( NonRESTfulResponse,
    mkNonRESTful200,
    mkNonRESTful301,
    mkNonRESTful400,
    mkNonRESTful404,
    mkNonRESTful503,
  )
where

import Data.Aeson (ToJSON, object, toJSON, (.=))
import qualified Data.Aeson.Key as Key
import Data.Text.Encoding (decodeUtf8Lenient)
import Network.HTTP.Types.Status
  ( Status,
    badRequest400,
    movedPermanently301,
    notFound404,
    ok200,
    serviceUnavailable503,
    statusCode,
    statusMessage,
  )
import Network.URI (URI)

data NonRESTfulResponse a = (ToJSON a) =>
  NonRESTfulResponse
  { nrrStatus :: Status,
    nrrBodyKey :: String,
    nrrBody :: a,
    nrrRedirectLocation :: Maybe URI
  }

mkNonRESTful200 :: (ToJSON a) => String -> a -> NonRESTfulResponse a
mkNonRESTful200 key body =
  NonRESTfulResponse
    { nrrStatus = ok200,
      nrrBodyKey = key,
      nrrBody = body,
      nrrRedirectLocation = Nothing
    }

mkNonRESTful301 :: (ToJSON a) => String -> a -> URI -> NonRESTfulResponse a
mkNonRESTful301 key body redirectLocation =
  NonRESTfulResponse
    { nrrStatus = movedPermanently301,
      nrrBodyKey = key,
      nrrBody = body,
      nrrRedirectLocation = Just redirectLocation
    }

mkNonRESTful400 :: String -> NonRESTfulResponse String
mkNonRESTful400 message =
  NonRESTfulResponse
    { nrrStatus = badRequest400,
      nrrBodyKey = "error",
      nrrBody = message,
      nrrRedirectLocation = Nothing
    }

mkNonRESTful404 :: String -> NonRESTfulResponse String
mkNonRESTful404 message =
  NonRESTfulResponse
    { nrrStatus = notFound404,
      nrrBodyKey = "error",
      nrrBody = message,
      nrrRedirectLocation = Nothing
    }

mkNonRESTful503 :: String -> NonRESTfulResponse String
mkNonRESTful503 message =
  NonRESTfulResponse
    { nrrStatus = serviceUnavailable503,
      nrrBodyKey = "error",
      nrrBody = message,
      nrrRedirectLocation = Nothing
    }

-- JSON
instance (ToJSON a) => ToJSON (NonRESTfulResponse a) where
  toJSON r =
    object
      [ "status" .= statusCode status,
        "statusText" .= decodeUtf8Lenient (statusMessage status),
        bodyKey .= toJSON (nrrBody r),
        "redirectLocation" .= redirectLocation
      ]
    where
      bodyKey = Key.fromString $ nrrBodyKey r
      redirectLocation = toJSON (show <$> nrrRedirectLocation r)
      status = nrrStatus r
