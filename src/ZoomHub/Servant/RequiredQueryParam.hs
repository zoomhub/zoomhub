{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE TypeOperators       #-}

module ZoomHub.Servant.RequiredQueryParam
  ( RequiredQueryParam
  ) where

import           Data.Proxy              (Proxy (Proxy))
import           Data.String.Conversions (cs)
import           Data.Typeable           (Typeable)
import           GHC.TypeLits            (KnownSymbol, Symbol, symbolVal)
import           Network.HTTP.Types      (parseQueryText)
import           Network.Wai             (rawQueryString)
import           Servant                 ((:>))
import           Servant.Common.Text     (FromText, fromText)
import           Servant.Server.Internal (HasServer, RouteMismatch (NotFound),
                                          ServerT, failWith, route)

data RequiredQueryParam (sym :: Symbol) a deriving Typeable

instance (KnownSymbol sym, FromText a, HasServer sublayout)
      => HasServer (RequiredQueryParam sym a :> sublayout) where

  type ServerT (RequiredQueryParam sym a :> sublayout) m =
    a -> ServerT sublayout m

  route Proxy subserver request respond = do
    let querytext = parseQueryText $ rawQueryString request
        maybeParam =
          case lookup paramname querytext of
            Nothing       -> Nothing -- param absent from the query string
            Just Nothing  -> Nothing -- param present with no value -> Nothing
            Just (Just v) -> fromText v -- if present, we try to convert to
                                        -- the right type
    case maybeParam of
      Nothing    -> respond $ failWith NotFound
      Just param ->
        route (Proxy :: Proxy sublayout) (subserver param) request respond

    where paramname = cs $ symbolVal (Proxy :: Proxy sym)
