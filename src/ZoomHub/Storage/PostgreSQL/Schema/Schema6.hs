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
    UsersTable0,
  )
where

import Squeal.PostgreSQL
  ( Definition,
    IsoQ (..),
    Join,
    NP ((:*)),
    NullType (NotNull, Null),
    Optionality (Def, NoDef),
    PGType (PGbool, PGint8, PGtext, PGtimestamptz),
    Public,
    SchemumType (Table),
    TableConstraint (PrimaryKey, Unique),
    as,
    bigserial,
    bool,
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

migration :: Migration (IsoQ Definition) Schemas5 Schemas6
migration =
  Migration
    "2024-10-12-1-add-users-table"
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

    teardown :: Definition Schemas6 Schemas5
    teardown = dropTable #users
