{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module ZoomHub.Servant.RawCapture
  ( RawCapture,
  )
where

import Data.Proxy (Proxy (Proxy))
import Data.Typeable (Typeable)
import GHC.TypeLits (KnownSymbol, Symbol)
import Servant (FromHttpApiData)
import Servant.API ((:>))
import Servant.Server.Internal
  ( HasServer,
    Router' (RawCaptureRouter),
    ServerT,
    hoistServerWithContext,
    route,
  )
import Servant.Server.Internal.Delayed (addCapture)
import Servant.Server.Internal.DelayedIO (delayedFail)
import Servant.Server.Internal.ServerError (err400)
import Web.HttpApiData (parseUrlPieceMaybe)

data RawCapture (sym :: Symbol) (a :: *) deriving (Typeable)

instance
  (KnownSymbol capture, FromHttpApiData a, HasServer api context) =>
  HasServer (RawCapture capture a :> api) context
  where

  type
    ServerT (RawCapture capture a :> api) m =
      a -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route Proxy context d =
    RawCaptureRouter $
      route
        (Proxy :: Proxy api)
        context
        ( addCapture d $ \txt -> case parseUrlPieceMaybe txt of
            Nothing -> delayedFail err400
            Just v -> return v
        )
