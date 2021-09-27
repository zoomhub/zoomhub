{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module ZoomHub.Storage.PostgreSQL.Schema
  ( Schemas,

    -- * Migrations
    migrations,
  )
where

import Squeal.PostgreSQL
  ( AlignedList (Done, (:>>)),
    Definition,
    Public,
    (>>>),
  )
import Squeal.PostgreSQL.Migration (Migration (..))
import qualified ZoomHub.Storage.PostgreSQL.Schema.Schema0 as Schema0
import qualified ZoomHub.Storage.PostgreSQL.Schema.Schema1 as Schema1
import qualified ZoomHub.Storage.PostgreSQL.Schema.Schema2 as Schema2
import qualified ZoomHub.Storage.PostgreSQL.Schema.Schema3 as Schema3
import ZoomHub.Storage.PostgreSQL.Schema.Schema4 (Schema4)
import qualified ZoomHub.Storage.PostgreSQL.Schema.Schema4 as Schema4

type Schemas = Public Schema4

migrations :: String -> AlignedList (Migration Definition) (Public '[]) Schemas
migrations hashidsSecret =
  Schema0.migrations hashidsSecret
    >>> Schema1.migration
    :>> Schema2.migration
    :>> Schema3.migration
    :>> Schema4.migration
    :>> Done
