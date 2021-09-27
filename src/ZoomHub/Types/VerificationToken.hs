{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ZoomHub.Types.VerificationToken
  ( VerificationToken,
    -- TODO: Can we test this without exporting it?
    unVerificationToken,
    fromText,
  )
where

import Data.Aeson (ToJSON (toJSON))
import Data.Aeson.Types (Value (String))
import Data.Maybe (fromJust)
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Servant (FromHttpApiData, parseUrlPiece)
import Squeal.PostgreSQL (FromValue (..), PG, PGType (PGtext), ToParam (..))

newtype VerificationToken = VerificationToken {unVerificationToken :: UUID}
  deriving (Eq)

instance Show VerificationToken where
  show = UUID.toString . unVerificationToken

fromText :: Text -> Maybe VerificationToken
fromText t = VerificationToken <$> UUID.fromText t

-- Text
instance FromHttpApiData VerificationToken where
  parseUrlPiece t =
    case UUID.fromText t of
      Just result ->
        Right $ VerificationToken result
      Nothing ->
        Left "Invalid verification token"

-- JSON
instance ToJSON VerificationToken where
  toJSON = String . UUID.toText . unVerificationToken

-- Squeal / PostgreSQL
type instance PG VerificationToken = 'PGtext

instance ToParam VerificationToken 'PGtext where
  toParam = toParam . UUID.toText . unVerificationToken

-- TODO: Unsafe use of `fromJust`:
instance FromValue 'PGtext VerificationToken where
  fromValue = VerificationToken . fromJust . UUID.fromText <$> fromValue @'PGtext
