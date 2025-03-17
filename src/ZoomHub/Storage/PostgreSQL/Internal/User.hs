{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
-- Simplify Squeal query type signatures
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -O0 #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -fomit-interface-pragmas #-}

{-# HLINT ignore "Use <$>" #-}

module ZoomHub.Storage.PostgreSQL.Internal.User where

import Squeal.PostgreSQL
  ( ConflictAction (DoNothing),
    ConflictClause (OnConflict),
    ConflictTarget (OnConstraint),
    DecodeRow,
    GenericParams (genericParams),
    NP (Nil, (:*)),
    NullType (NotNull, Null),
    Optional (Default, Set),
    PGType (PGbool, PGint8, PGtext, PGtimestamptz),
    Statement (Manipulation),
    UsingClause (Using),
    as,
    currentTimestamp,
    ilike,
    insertInto,
    isNotNull,
    just_,
    manipulation,
    null_,
    param,
    table,
    update,
    (!),
    (.&&),
    (:::),
  )
import Squeal.PostgreSQL.Manipulation (pattern Returning_)
import Squeal.PostgreSQL.Manipulation.Insert (pattern Values_)
import ZoomHub.Storage.PostgreSQL.Schema (Schemas)
import ZoomHub.Types.User (User (User))
import qualified ZoomHub.Types.User as User

-- user
type UserRow =
  '[ "id" ::: 'NotNull 'PGint8,
     "kinde_user_id" ::: 'NotNull 'PGtext,
     "email" ::: 'NotNull 'PGtext,
     "is_email_verified" ::: 'NotNull 'PGbool,
     "given_name" ::: 'Null 'PGtext,
     "family_name" ::: 'Null 'PGtext,
     "image_url" ::: 'Null 'PGtext,
     "updated_at" ::: 'NotNull 'PGtimestamptz,
     "created_at" ::: 'NotNull 'PGtimestamptz
   ]

-- Returns existing user based on `email` or create a new one.
findOrCreate :: Statement Schemas User User
findOrCreate = Manipulation encode decode sql
  where
    encode = genericParams
    decode = decodeUser
    sql =
      insertInto
        #users
        ( Values_
            ( (Default `as` #id)
                :* (Set (param @1) `as` #kinde_user_id)
                :* (Set (param @2) `as` #email)
                :* (Set (param @3) `as` #is_email_verified)
                :* (Set (param @4) `as` #given_name)
                :* (Set (param @5) `as` #family_name)
                :* (Set (param @6) `as` #image_url)
                :* (Set currentTimestamp `as` #updated_at)
                :* (Default `as` #created_at)
            )
        )
        (OnConflict (OnConstraint #users_unique_email) DoNothing)
        ( Returning_
            ( #id
                :* #kinde_user_id
                :* #email
                :* #is_email_verified
                :* #given_name
                :* #family_name
                :* #image_url
                :* #updated_at
                :* #created_at
            )
        )

-- Links all existing verified content for this user.
linkVerifiedContent :: Statement Schemas () ()
linkVerifiedContent = manipulation sql
  where
    sql =
      update
        (#content `as` #c)
        ( (Set (just_ (#u ! #id)) `as` #user_id)
            :* (Set null_ `as` #submitter_email)
        )
        (Using (table (#users `as` #u)))
        ( ((#c ! #submitter_email) `ilike` just_ (#u ! #email))
            .&& isNotNull (#c ! #verified_at)
        )
        (Returning_ Nil)

decodeUser :: DecodeRow UserRow User
decodeUser = do
  kindeUserId <- #kinde_user_id
  email <- #email
  isEmailVerified <- #is_email_verified
  givenName <- #given_name
  familyName <- #family_name
  imageURL <- #image_url

  return User {..}
