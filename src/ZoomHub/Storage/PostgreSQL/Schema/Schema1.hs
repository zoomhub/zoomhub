{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module ZoomHub.Storage.PostgreSQL.Schema.Schema1
  ( migration,
  )
where

import Squeal.PostgreSQL
  ( Definition,
    addColumn,
    alterTable,
    dropColumn,
    nullable,
    text,
    (&),
  )
import Squeal.PostgreSQL.Migration (Migration (..))
import ZoomHub.Storage.PostgreSQL.Schema.Schema0 (Schemas0)

migration :: Migration Definition Schemas0 _
migration =
  Migration
    { name = "2021-02-15-1: Add submitter email",
      up = setup,
      down = teardown
    }

setup :: Definition Schemas0 _
setup = alterTable #content (addColumn #submitter_email (text & nullable))

teardown :: Definition _ Schemas0
teardown = alterTable #content (dropColumn #submitter_email)
