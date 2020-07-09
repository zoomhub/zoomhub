{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ZoomHub.Servant.RequiredQueryParam
  ( RequiredQueryParam,
  )
where

import Control.Monad (join)
import Data.Proxy (Proxy (Proxy))
import Data.String.Conversions (cs)
import Data.Text as T
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)
import Network.HTTP.Types (parseQueryText)
import Network.Wai (Request, rawQueryString)
import Servant (FromHttpApiData)
import Servant.API ((:>))
import Servant.Server.Internal
  ( HasServer,
    ServerT,
    hoistServerWithContext,
    route,
  )
import Servant.Server.Internal.Delayed (addParameterCheck)
import Servant.Server.Internal.DelayedIO (DelayedIO, delayedFail, withRequest)
import Servant.Server.Internal.ServerError (err400)
import Web.HttpApiData (parseQueryParam)

data RequiredQueryParam (sym :: Symbol) a deriving (Typeable)

instance
  (KnownSymbol sym, FromHttpApiData a, HasServer api context) =>
  HasServer (RequiredQueryParam sym a :> api) context
  where

  type
    ServerT (RequiredQueryParam sym a :> api) m =
      a -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context subserver =
    let querytext req = parseQueryText $ rawQueryString req
        paramname = cs $ symbolVal (Proxy :: Proxy sym)
        parseParam :: Request -> DelayedIO a
        parseParam req = case mev of
          Just (Right x) -> return x
          _ -> delayedFail err400 -- Skip to next handler if param not present
          where
            mev :: Maybe (Either T.Text a)
            mev = fmap parseQueryParam $ join $ lookup paramname $ querytext req
        delayed = addParameterCheck subserver . withRequest $ parseParam
     in route (Proxy :: Proxy api) context delayed
