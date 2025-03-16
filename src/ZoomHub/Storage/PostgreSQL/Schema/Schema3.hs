{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module ZoomHub.Storage.PostgreSQL.Schema.Schema3
  ( Schema3,
    Schemas3,
    migration,
  )
where

import Squeal.PostgreSQL
  ( Definition,
    IsoQ (..),
    NullType (NotNull, Null),
    Optionality (Def, NoDef),
    PGType (PGfloat8, PGint4, PGint8, PGtext, PGtimestamptz),
    Public,
    SchemumType (Table),
    TableConstraint (PrimaryKey, Unique),
    addColumn,
    alterTable,
    default_,
    dropColumn,
    null_,
    nullable,
    timestampWithTimeZone,
    (&),
    (:::),
    (:=>),
  )
import Squeal.PostgreSQL.Session.Migration (Migration (..))
import ZoomHub.Storage.PostgreSQL.Schema.Schema0 (ConfigTable0, FlickrTable0, ImageTable0)

type Schema3 =
  '[ ConfigTable0,
     ContentTable3,
     ImageTable0,
     FlickrTable0
   ]

type Schemas3 = Public Schema3

type ContentTable3 =
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
                     "verification_token" ::: 'NoDef :=> 'Null 'PGtext,
                     "verified_at" ::: 'Def :=> 'Null 'PGtimestamptz
                   ]
          )

migration :: Migration (IsoQ Definition) _ Schemas3
migration =
  Migration
    "2021-09-13-1: Add `verified_at`"
    IsoQ
      { up = setup,
        down = teardown
      }

setup :: Definition _ Schemas3
setup =
  alterTable
    #content
    ( addColumn
        #verified_at
        (timestampWithTimeZone & nullable & default_ null_)
    )

teardown :: Definition Schemas3 _
teardown = alterTable #content (dropColumn #verified_at)
