{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module ZoomHub.Types.VerificationToken
  ( VerificationToken,
    VerificationToken' (VerificationToken),
    -- TODO: Can we test this without exporting it?
    unVerificationToken,
  )
where

import Data.Maybe (fromJust)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import Servant (FromHttpApiData, parseUrlPiece)
import Squeal.PostgreSQL (FromValue (..), PG, PGType (PGtext), ToParam (..))

newtype VerificationToken' a = VerificationToken {unVerificationToken :: a}
  deriving (Eq, Functor)

type VerificationToken = VerificationToken' UUID

instance Show VerificationToken where
  show = UUID.toString . unVerificationToken

-- Text
instance FromHttpApiData VerificationToken where
  parseUrlPiece t =
    case UUID.fromText t of
      Just result ->
        Right $ VerificationToken result
      Nothing ->
        Left "Invalid verification token"

-- Squeal / PostgreSQL
type instance PG VerificationToken = 'PGtext

instance ToParam VerificationToken 'PGtext where
  toParam = toParam . UUID.toText . unVerificationToken

-- TODO: Unsafe use of `fromJust`:
instance FromValue 'PGtext VerificationToken where
  fromValue = VerificationToken . fromJust . UUID.fromText <$> fromValue @'PGtext
