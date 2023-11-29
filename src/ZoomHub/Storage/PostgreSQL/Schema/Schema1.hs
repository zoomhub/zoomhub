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
    IsoQ(..),
  )
import Squeal.PostgreSQL.Session.Migration (Migration (..))
import ZoomHub.Storage.PostgreSQL.Schema.Schema0 (Schemas0)

migration :: Migration (IsoQ Definition) Schemas0 _
migration =
  Migration "2021-02-15-1: Add submitter email" IsoQ
    { up = setup,
      down = teardown
    }

setup :: Definition Schemas0 _
setup = alterTable #content (addColumn #submitter_email (text & nullable))

teardown :: Definition _ Schemas0
teardown = alterTable #content (dropColumn #submitter_email)
