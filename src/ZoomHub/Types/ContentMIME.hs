{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ZoomHub.Types.ContentMIME
  ( ContentMIME
  , ContentMIME'(ContentMIME)
  , unContentMIME
  , fromText
  ) where

import Codec.MIME.Parse (parseMIMEType)
import Codec.MIME.Type (Type, showType)
import Data.Aeson (ToJSON, Value(String), toJSON)
import Data.Maybe (fromJust)
import qualified Data.Text as T
import Squeal.PostgreSQL (FromValue(..), PG, PGType(PGtext), ToParam(..))


newtype ContentMIME' a = ContentMIME { unContentMIME :: a } deriving (Eq, Show)
type ContentMIME = ContentMIME' Type

toText :: ContentMIME -> T.Text
toText = showType . unContentMIME

fromText :: T.Text -> Maybe ContentMIME
fromText t = ContentMIME <$> parseMIMEType t

-- JSON
instance ToJSON ContentMIME where
  toJSON = String . toText

-- Squeal / PostgreSQL
type instance PG ContentMIME = 'PGtext
instance ToParam ContentMIME 'PGtext where
  toParam = toParam . toText

instance FromValue 'PGtext ContentMIME where
  -- TODO: What if database value is not a valid MIME type?
  fromValue = ContentMIME . fromJust . parseMIMEType <$> fromValue @'PGtext
