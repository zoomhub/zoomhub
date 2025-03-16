{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module ZoomHub.Storage.PostgreSQL.Schema.Schema6
  ( Schema6,
    Schemas6,
    migration,
  )
where

import Squeal.PostgreSQL
  ( Aliasable (as),
    Definition,
    IndexType (Btree),
    IsoQ (..),
    NP ((:*)),
    NullType (NotNull, Null),
    OnDeleteClause (OnDelete),
    OnUpdateClause (OnUpdate),
    Optionality (Def, NoDef),
    PGType (PGbool, PGfloat8, PGint4, PGint8, PGtext, PGtimestamptz),
    Public,
    ReferentialAction (Cascade),
    SchemumType (Index, Table),
    TableConstraint (ForeignKey, PrimaryKey, Unique),
    addColumn,
    addConstraint,
    alterTable,
    bigint,
    bigserial,
    bool,
    createTable,
    currentTimestamp,
    default_,
    dropColumn,
    dropConstraint,
    dropTable,
    foreignKey,
    notNullable,
    null_,
    nullable,
    primaryKey,
    text,
    timestampWithTimeZone,
    unique,
    (&),
    (:::),
    (:=>),
    (>>>),
  )
import Squeal.PostgreSQL.Session.Migration (Migration (..))
import ZoomHub.Storage.PostgreSQL.Schema.Schema0 (ConfigTable0, FlickrTable0, ImageTable0)
import ZoomHub.Storage.PostgreSQL.Schema.Schema5 (Schemas5)

type Schema6 =
  [ ConfigTable0,
    ContentTable4,
    ImageTable0,
    FlickrTable0,
    "content_active_at_idx" ::: 'Index 'Btree,
    "content_initialized_at_idx" ::: 'Index 'Btree,
    "content_num_views_idx" ::: 'Index 'Btree,
    "content_state_idx" ::: 'Index 'Btree,
    "content_submitter_email_idx" ::: 'Index 'Btree,
    "content_verified_at_idx" ::: 'Index 'Btree,
    "content_version_idx" ::: 'Index 'Btree,
    UsersTable0
  ]

type Schemas6 = Public Schema6

type UsersTable0 =
  "users"
    ::: 'Table
          ( '[ "pk_users" ::: 'PrimaryKey '["id"],
               "users_unique_kinde_user_id" ::: 'Unique '["kinde_user_id"],
               "users_unique_email" ::: 'Unique '["email"]
             ]
              :=> '[ "id" ::: 'Def :=> 'NotNull 'PGint8,
                     "kinde_user_id" ::: 'NoDef :=> 'NotNull 'PGtext,
                     "email" ::: 'NoDef :=> 'NotNull 'PGtext,
                     "is_email_verified" ::: 'NoDef :=> 'NotNull 'PGbool,
                     "given_name" ::: 'NoDef :=> 'Null 'PGtext,
                     "family_name" ::: 'NoDef :=> 'Null 'PGtext,
                     "image_url" ::: 'NoDef :=> 'Null 'PGtext,
                     "updated_at" ::: 'NoDef :=> 'NotNull 'PGtimestamptz,
                     "created_at" ::: 'Def :=> 'NotNull 'PGtimestamptz
                   ]
          )

type ContentTable4 =
  "content"
    ::: 'Table
          ( '[ "pk_content" ::: 'PrimaryKey '["id"],
               "content_unique_hash_id" ::: 'Unique '["hash_id"],
               "content_unique_url" ::: 'Unique '["url"],
               "fk_user_id" ::: 'ForeignKey '["user_id"] "public" "users" '["id"]
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
                     "verified_at" ::: 'Def :=> 'Null 'PGtimestamptz,
                     "user_id" ::: 'Def :=> 'Null PGint8
                   ]
          )

migration :: Migration (IsoQ Definition) Schemas5 Schemas6
migration =
  Migration
    "2025-03-16-1-add-users-table"
    IsoQ
      { up = setup,
        down = teardown
      }

setup :: Definition Schemas5 Schemas6
setup =
  createTable
    #users
    ( (bigserial `as` #id)
        :* ((text & notNullable) `as` #kinde_user_id)
        :* ((text & notNullable) `as` #email)
        :* ((bool & notNullable) `as` #is_email_verified)
        :* ((text & nullable) `as` #given_name)
        :* ((text & nullable) `as` #family_name)
        :* ((text & nullable) `as` #image_url)
        :* ((timestampWithTimeZone & notNullable) `as` #updated_at)
        :* ((timestampWithTimeZone & notNullable & default_ currentTimestamp) `as` #created_at)
    )
    ( (primaryKey #id `as` #pk_users)
        :* (unique #kinde_user_id `as` #users_unique_kinde_user_id)
        :* (unique #email `as` #users_unique_email)
    )
    >>> alterTable #content (addColumn #user_id (bigint & nullable & default_ null_))
    >>> alterTable
      #content
      ( addConstraint
          #fk_user_id
          ( foreignKey
              #user_id
              #users
              #id
              (OnDelete Cascade)
              (OnUpdate Cascade)
          )
      )

teardown :: Definition Schemas6 Schemas5
teardown =
  alterTable #content (dropColumn #user_id)
    >>> alterTable #content (dropConstraint #fk_user_id)
    >>> dropTable #users
