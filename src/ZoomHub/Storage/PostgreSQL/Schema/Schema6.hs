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
    NP ((:*)),
    NullType (NotNull, Null),
    Optionality (Def, NoDef),
    PGType (PGint8, PGtext),
    Public,
    SchemumType (Table),
    TableConstraint (PrimaryKey, Unique),
    as,
    Join,
    bigserial,
    createTable,
    dropTable,
    notNullable,
    nullable,
    primaryKey,
    text,
    unique,
    (&),
    (:::),
    (:=>),
  )
import Squeal.PostgreSQL.Session.Migration (Migration (..))
import ZoomHub.Storage.PostgreSQL.Schema.Schema5 (Schema5, Schemas5)

type Schema6 = Join Schema5 '[ UsersTable0 ]

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
                     "family_name" ::: 'NoDef :=> 'Null 'PGtext
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
        )
        ( (primaryKey #id `as` #pk_users)
            :* (unique #email `as` #users_unique_email)
        )

    teardown :: Definition Schemas6 Schemas5
    teardown = dropTable #users
