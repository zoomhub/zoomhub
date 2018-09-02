{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module ZoomHub.API.ContentTypes.JavaScript
  ( ToJS
  , JavaScript
  , toJS
  ) where

import           Data.Aeson                 (ToJSON, encode)
import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.Typeable              (Typeable)
import           Network.HTTP.Media         ((//), (/:))
import           Servant.API.ContentTypes   (Accept (..), MimeRender (..))

import           ZoomHub.API.Types.Callback (unCallback)
import           ZoomHub.API.Types.JSONP    (JSONP, jsonpBody, jsonpCallback)

data JavaScript
  deriving (Typeable)

-- | @application/javascript;charset=utf-8@
instance Accept JavaScript where
  contentType _ = "application" // "javascript" /: ("charset", "utf-8")

instance ToJS a => MimeRender JavaScript a where
  mimeRender _ = BC.pack . toJS

-- | A type that can be converted to @application/javascript@
class ToJS a where
  toJS :: a -> String

instance ToJS String where
  toJS = id

instance ToJSON a => ToJS (JSONP a)
  -- The `/**/` is a specific security mitigation for
  -- ‘Rosetta Flash JSONP abuse.’
  -- The `typeof` check helps reduce client error noise.
  -- Adopted from Express.js: https://git.io/vaT9r
                                                   where
  toJS jsonp =
    "/**/ typeof " ++
    callback ++ " === 'function' && " ++ callback ++ "(" ++ body ++ ");"
    where
      callback = unCallback $ jsonpCallback jsonp
      body = BC.unpack . encode $ jsonpBody jsonp
