{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings         #-}

module ZoomHub.API.Types.NonRESTfulResponse
  ( NonRESTfulResponse
  , mkNonRESTful200
  , mkNonRESTful301
  ) where

import           Data.Aeson                (ToJSON, object, toJSON, (.=))
import qualified Data.Text                 as T
import           Data.Text.Encoding        (decodeUtf8)
import           Network.HTTP.Types.Status (Status, movedPermanently301, ok200,
                                            statusCode, statusMessage)
import           Network.URI               (URI)

data NonRESTfulResponse a = ToJSON a => NonRESTfulResponse
  { nrrStatus           :: Status
  , nrrBodyKey          :: String
  , nrrBody             :: a
  , nrrRedirectLocation :: Maybe URI
  }

mkNonRESTful200 :: ToJSON a => String -> a -> NonRESTfulResponse a
mkNonRESTful200 key body = NonRESTfulResponse
  { nrrStatus = ok200
  , nrrBodyKey = key
  , nrrBody = body
  , nrrRedirectLocation = Nothing
  }

mkNonRESTful301 :: ToJSON a => String -> a -> URI -> NonRESTfulResponse a
mkNonRESTful301 key body redirectLocation = NonRESTfulResponse
  { nrrStatus = movedPermanently301
  , nrrBodyKey = key
  , nrrBody = body
  , nrrRedirectLocation = Just redirectLocation
  }


instance ToJSON a => ToJSON (NonRESTfulResponse a) where
  toJSON r = object
      [ "status" .= statusCode status
      , "statusText" .= decodeUtf8 (statusMessage status)
      , bodyKey .= toJSON (nrrBody r)
      ]
    where
      bodyKey = T.pack (nrrBodyKey r)
      status = nrrStatus r
