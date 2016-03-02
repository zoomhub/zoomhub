{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module ZoomHub.API.ContentTypes
  ( ToJS
  , JavaScript
  , toJS
  ) where

import qualified Data.ByteString.Lazy.Char8 as BC
import           Data.Typeable              (Typeable)
import           Network.HTTP.Media         ((//), (/:))
import           Servant.API.ContentTypes   (Accept (..), MimeRender (..))

data JavaScript deriving Typeable

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
