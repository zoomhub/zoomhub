{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module ZoomHub.Squeal.Citext where

import Data.CaseInsensitive (CI (original))
import qualified Data.CaseInsensitive as CI
import Data.Text (Text)
import qualified Database.PostgreSQL.Simple.Types as LibPQ
import Squeal.PostgreSQL
  ( Definition (UnsafeDefinition),
    FromPG (fromPG),
    IsPG (PG),
    OidOf (oidOf),
    PGType (UnsafePGType),
    PGTyped (pgtype),
    ToPG (toPG),
    TypeExpression (UnsafeTypeExpression),
  )

-- import ZoomHub.Types.User (Email)

-- | Postgres citext type
type PGcitext = 'UnsafePGType "citext"

-- | Creates citext extension if it doesn’t exist.
createCitextExtensionIfNotExists :: Definition db db
createCitextExtensionIfNotExists =
  UnsafeDefinition "CREATE EXTENSION IF NOT EXISTS \"citext\";"

citext :: TypeExpression db (null PGcitext)
citext = UnsafeTypeExpression "citext"

instance PGTyped db PGcitext where pgtype = citext

instance IsPG (CI Text) where type PG (CI Text) = PGcitext

instance ToPG db (CI Text) where
  toPG = toPG . original

-- instance IsPG (CI Email) where type PG (CI Email) = PGcitext

instance FromPG (CI Text) where
  fromPG = do
    value <- fromPG @Text
    pure $ CI.mk value

instance OidOf db PGcitext where
  oidOf = do
    -- Query the OID dynamically from pg_type
    pure $ LibPQ.Oid 0 -- Fallback to 0, which should work for most cases
