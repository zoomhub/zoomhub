{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module ZoomHub.Storage.PostgreSQL.Schema.Schema4
  ( Schema4,
    migration,
  )
where

import Squeal.PostgreSQL
  ( Definition,
    Manipulation (UnsafeManipulation),
    Public,
    manipulation_,
    IsoQ(..),
  )
import Squeal.PostgreSQL.Session.Migration (Migration (..))
import Text.RawString.QQ (r)
import ZoomHub.Storage.PostgreSQL.Schema.Schema3 (Schema3)

type Schema4 = Schema3

type Schemas4 = Public Schema4

migration :: Migration (IsoQ Definition) Schemas4 Schemas4
migration =
  Migration "2021-09-28-1: Add content indexes" IsoQ
    { up = setup,
      down = teardown
    }
  where
    -- TODO: Use `createIndex` once we upgraded to Squeal 0.6:
    setup :: Definition Schemas4 Schemas4
    setup =
      manipulation_ $
        UnsafeManipulation
          [r|
            CREATE INDEX "content_active_at_idx" ON "content" USING btree ("active_at");
            CREATE INDEX "content_initialized_at_idx" ON "content" USING btree ("initialized_at");
            CREATE INDEX "content_num_views_idx" ON "content" USING btree ("num_views" DESC NULLS LAST);
            CREATE INDEX "content_state_idx" ON "content" USING btree ("state");
            CREATE INDEX "content_verified_at_idx" ON "content" USING btree ("verified_at");
            CREATE INDEX "content_version_idx" ON "content" USING btree ("version" DESC NULLS LAST);
          |]

    -- TODO: Use `dropIndex` once we upgraded to Squeal 0.6:
    teardown :: Definition Schemas4 Schemas4
    teardown =
      manipulation_ $
        UnsafeManipulation
          [r|
            DROP INDEX "content_active_at_idx";
            DROP INDEX "content_initialized_at_idx";
            DROP INDEX "content_num_views_idx";
            DROP INDEX "content_state_idx";
            DROP INDEX "content_verified_at_idx";
            DROP INDEX "content_version_idx";
          |]
