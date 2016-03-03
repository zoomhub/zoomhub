{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module ZoomHub.Servant.RawCapture
  ( RawCapture
  ) where


import           Data.Proxy              (Proxy (Proxy))
import           Data.Text               (Text)
import qualified Data.Text               as T
import qualified Data.Text.Encoding      as T
import           Data.Typeable           (Typeable)
import           GHC.TypeLits            (KnownSymbol, Symbol)
import           Network.Wai             (pathInfo, rawPathInfo)
import           Servant                 ((:>))
import           Servant.Common.Text     (FromText, fromText)
import           Servant.Server.Internal (HasServer, RouteMismatch (NotFound),
                                          ServerT, failWith, route)

data RawCapture (sym :: Symbol) a deriving (Typeable)

capturedRaw :: FromText a => proxy (RawCapture sym a) -> Text -> Maybe a
capturedRaw _ = fromText

instance (KnownSymbol capture, FromText a, HasServer sublayout)
      => HasServer (RawCapture capture a :> sublayout) where

  type ServerT (RawCapture capture a :> sublayout) m =
     a -> ServerT sublayout m

  route Proxy subserver request respond =
    let rawWithLeadingSlash = T.decodeUtf8 (rawPathInfo request)
        raw = T.tail rawWithLeadingSlash in
    case capturedRaw captureProxy raw of
           Nothing  -> respond $ failWith NotFound
           Just v   -> route (Proxy :: Proxy sublayout) (subserver v) request{
                         -- Finish routing as we captured entire path:
                         pathInfo = [""]
                       } respond

    where captureProxy = Proxy :: Proxy (RawCapture capture a)
