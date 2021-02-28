{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -fomit-interface-pragmas -fno-specialise -fno-full-laziness #-}

module ZoomHub.Storage.PostgreSQL.Schema.Schema2
  ( Schema2,
    migration,
  )
where

import Squeal.PostgreSQL
  ( (&),
    (:::),
    (:=>),
    ColumnConstraint (Def, NoDef),
    Definition,
    NullityType (NotNull, Null),
    PGType (PGfloat8, PGint4, PGint8, PGtext, PGtimestamptz),
    Public,
    SchemumType (Table),
    TableConstraint (PrimaryKey, Unique),
    addColumn,
    alterTable,
    dropColumn,
    nullable,
    text,
  )
import Squeal.PostgreSQL.Migration (Migration (..))
import ZoomHub.Storage.PostgreSQL.Schema.Schema0 (ConfigTable0, FlickrTable0, ImageTable0)

type Schema2 =
  '[ ConfigTable0,
     ContentTable2,
     ImageTable0,
     FlickrTable0
   ]

type Schemas2 = Public Schema2

type ContentTable2 =
  "content"
    ::: 'Table
          ( '[ "pk_content" ::: 'PrimaryKey '["id"],
               "content_unique_hash_id" ::: 'Unique '["hash_id"],
               "content_unique_url" ::: 'Unique '["url"]
             ]
              :=> '[ "id" ::: 'Def :=> 'NotNull 'PGint8,
                     "hash_id" ::: 'NoDef :=> 'NotNull 'PGtext,
                     "type_id" ::: 'Def :=> 'NotNull 'PGint4,
                     "url" ::: 'NoDef :=> 'NotNull 'PGtext,
                     "state" ::: 'Def :=> 'NotNull 'PGtext,
                     "initialized_at" ::: 'Def :=> 'NotNull 'PGtimestamptz,
                     "active_at" ::: 'Def :=> 'Null 'PGtimestamptz,
                     "completed_at" ::: 'Def :=> 'Null 'PGtimestamptz,
                     "title" ::: 'Def :=> 'Null 'PGtext,
                     "attribution_text" ::: 'Def :=> 'Null 'PGtext,
                     "attribution_link" ::: 'Def :=> 'Null 'PGtext,
                     "mime" ::: 'Def :=> 'Null 'PGtext,
                     "size" ::: 'Def :=> 'Null 'PGint8,
                     "error" ::: 'Def :=> 'Null 'PGtext,
                     "progress" ::: 'Def :=> 'NotNull 'PGfloat8,
                     "abuse_level_id" ::: 'Def :=> 'NotNull 'PGint4,
                     "num_abuse_reports" ::: 'Def :=> 'NotNull 'PGint8,
                     "num_views" ::: 'Def :=> 'NotNull 'PGint8,
                     "version" ::: 'Def :=> 'NotNull 'PGint4,
                     "submitter_email" ::: 'NoDef :=> 'Null 'PGtext,
                     "verification_token" ::: 'NoDef :=> 'Null 'PGtext
                   ]
          )

migration :: Migration Definition _ Schemas2
migration = Migration
  { name = "2021-02-28-1: Add verification token",
    up = setup,
    down = teardown
  }

setup :: Definition _ Schemas2
setup = alterTable #content (addColumn #verification_token (text & nullable))

teardown :: Definition Schemas2 _
teardown = alterTable #content (dropColumn #verification_token)
