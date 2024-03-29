{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ZoomHub.Types.ContentMIME
  ( ContentMIME,
    ContentMIME' (ContentMIME),
    unContentMIME,
    fromText,
  )
where

import Codec.MIME.Parse (parseMIMEType)
import Codec.MIME.Type (Type, showType)
import Data.Aeson (ToJSON, Value (String), toJSON)
import Data.Maybe (fromJust)
import Data.Text (Text)
import qualified Data.Text as T
import Squeal.PostgreSQL (FromPG (..), Inline, IsPG, PG, PGType (PGtext), ToPG (..), inline)

newtype ContentMIME' a = ContentMIME {unContentMIME :: a}
  deriving stock (Eq, Show)

type ContentMIME = ContentMIME' Type

toText :: ContentMIME -> T.Text
toText = showType . unContentMIME

fromText :: T.Text -> Maybe ContentMIME
fromText t = ContentMIME <$> parseMIMEType t

-- JSON
instance ToJSON ContentMIME where
  toJSON = String . toText

-- Squeal / PostgreSQL
instance IsPG ContentMIME where
  type PG ContentMIME = 'PGtext

instance FromPG ContentMIME where
  fromPG = ContentMIME . fromJust . parseMIMEType <$> fromPG @Text

instance ToPG db ContentMIME where
  toPG = toPG . toText

instance Inline ContentMIME where
  inline = inline . toText
