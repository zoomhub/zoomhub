{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

module ZoomHub.Storage.PostgreSQL.Schema.Schema6
  ( Schema6,
    migration,
  )
where

import Squeal.PostgreSQL
  ( Definition,
    IsoQ (..),
    Join,
    NP ((:*)),
    NullType (NotNull, Null),
    Optionality (Def, NoDef),
    PGType (PGint8, PGtext, PGtimestamptz),
    Public,
    SchemumType (Table),
    TableConstraint (PrimaryKey, Unique),
    as,
    bigserial,
    createTable,
    currentTimestamp,
    default_,
    dropTable,
    notNullable,
    nullable,
    primaryKey,
    text,
    timestampWithTimeZone,
    unique,
    (&),
    (:::),
    (:=>),
  )
import Squeal.PostgreSQL.Session.Migration (Migration (..))
import ZoomHub.Storage.PostgreSQL.Schema.Schema5 (Schema5, Schemas5)

type Schema6 = Join Schema5 '[UsersTable0]

type Schemas6 = Public Schema6

type UsersTable0 =
  "users"
    ::: 'Table
          ( '[ "pk_users" ::: 'PrimaryKey '["id"],
               "users_unique_email" ::: 'Unique '["email"]
             ]
              :=> '[ "id" ::: 'Def :=> 'NotNull 'PGint8,
                     "email" ::: 'NoDef :=> 'NotNull 'PGtext,
                     "given_name" ::: 'NoDef :=> 'Null 'PGtext,
                     "family_name" ::: 'NoDef :=> 'Null 'PGtext,
                     "image_url" ::: 'NoDef :=> 'Null 'PGtext,
                     "updated_at" ::: 'NoDef :=> 'NotNull 'PGtimestamptz,
                     "created_at" ::: 'Def :=> 'NotNull 'PGtimestamptz
                   ]
          )

migration :: Migration (IsoQ Definition) Schemas5 Schemas6
migration =
  Migration
    "2024-10-12-1: Add users table"
    IsoQ
      { up = setup,
        down = teardown
      }
  where
    setup :: Definition Schemas5 Schemas6
    setup =
      createTable
        #users
        ( (bigserial `as` #id)
            :* ((text & notNullable) `as` #email)
            :* ((text & nullable) `as` #given_name)
            :* ((text & nullable) `as` #family_name)
            :* ((text & nullable) `as` #image_url)
            :* ((timestampWithTimeZone & notNullable) `as` #updated_at)
            :* ((timestampWithTimeZone & notNullable & default_ currentTimestamp) `as` #created_at)
        )
        ( (primaryKey #id `as` #pk_users)
            :* (unique #email `as` #users_unique_email)
        )

    teardown :: Definition Schemas6 Schemas5
    teardown = dropTable #users
