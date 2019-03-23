{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module ZoomHub.Storage.PostgreSQL2.Schema
    ( Schema
    -- * Testing
    , setup
    , teardown
    ) where

import Squeal.PostgreSQL
  ( (:::)
  , (:=>)
  , ColumnConstraint(Def, NoDef)
  , Definition
  , NP((:*))
  , NullityType(NotNull, Null)
  , OnDeleteClause(OnDeleteCascade)
  , OnUpdateClause(OnUpdateCascade)
  , PGType(PGbool, PGfloat8, PGint4, PGint8, PGtext, PGtimestamptz)
  , SchemumType(Table)
  , TableConstraint(ForeignKey, PrimaryKey)
  , as
  , bigint
  , bigserial
  , bool
  , createTable
  , currentTimestamp
  , default_
  , doublePrecision
  , dropTable
  , foreignKey
  , int
  , int4
  , notNullable
  , nullable
  , primaryKey
  , text
  , timestampWithTimeZone
  , (&)
  , (>>>)
  )

type Schema =
    '[ ContentTable
     , ImageTable
     , FlickrTable
     ]

type ContentTable =
  "content" ::: 'Table
    ( '[ "pk_content" ::: 'PrimaryKey '["id"]
       ] :=>
        '[ "id" ::: 'Def :=> 'NotNull 'PGint8
         , "hash_id" ::: 'NoDef :=> 'NotNull 'PGtext
         , "type_id" ::: 'NoDef :=> 'NotNull 'PGint4
         , "url" ::: 'NoDef :=> 'NotNull 'PGtext
         , "state" ::: 'NoDef :=> 'NotNull 'PGtext
         , "initialized_at" ::: 'Def :=> 'NotNull 'PGtimestamptz
         , "active_at" ::: 'NoDef :=> 'Null 'PGtimestamptz
         , "completed_at" ::: 'NoDef :=> 'Null 'PGtimestamptz
         , "title" ::: 'NoDef :=> 'Null 'PGtext
         , "attribution_text" ::: 'NoDef :=> 'Null 'PGtext
         , "attribution_link" ::: 'NoDef :=> 'Null 'PGtext
         , "mime" ::: 'NoDef :=> 'Null 'PGtext
         , "size" ::: 'NoDef :=> 'Null 'PGint8
         , "error" ::: 'NoDef :=> 'Null 'PGtext
         , "progress" ::: 'Def :=> 'NotNull 'PGfloat8
         , "abuse_level_id" ::: 'Def :=> 'NotNull 'PGint4
         , "num_abuse_reports" ::: 'Def :=> 'NotNull 'PGint8
         , "num_views" ::: 'Def :=> 'NotNull 'PGint8
         , "version" ::: 'Def :=> 'NotNull 'PGint4
         ]
    )

type ImageTable =
  "image" ::: 'Table
    ( '[ "pk_image" ::: 'PrimaryKey '["content_id"]
       , "fk_content_id" ::: 'ForeignKey '["content_id"] "content" '["id"]
       ] :=>
        '[ "content_id" ::: 'Def :=> 'NotNull 'PGint8
         , "created_at" ::: 'Def :=> 'NotNull 'PGtimestamptz
         , "width" ::: 'NoDef :=> 'NotNull 'PGint8
         , "height" ::: 'NoDef :=> 'NotNull 'PGint8
         , "tile_size" ::: 'NoDef :=> 'NotNull 'PGint4
         , "tile_overlap" ::: 'NoDef :=> 'NotNull 'PGint4
         , "tile_format" ::: 'NoDef :=> 'NotNull 'PGtext
         ]
    )

type FlickrTable =
  "flickr" ::: 'Table
    ( '[ "pk_flickr" ::: 'PrimaryKey '["content_id"]
        , "fk_content_id" ::: 'ForeignKey '["content_id"] "content" '["id"]
        ] :=>
        '[ "content_id" ::: 'Def :=> 'NotNull 'PGint8
         , "farm_id" ::: 'NoDef :=> 'NotNull 'PGint4
         , "server_id" ::: 'NoDef :=> 'NotNull 'PGint4
         , "photo_id" ::: 'NoDef :=> 'NotNull 'PGtext
         , "secret" ::: 'NoDef :=> 'NotNull 'PGtext
         , "size_id" ::: 'NoDef :=> 'NotNull 'PGint4
         , "is_public" ::: 'NoDef :=> 'NotNull 'PGbool
         , "license_id" ::: 'NoDef :=> 'NotNull 'PGint4
         , "original_extension" ::: 'NoDef :=> 'Null 'PGtext
         , "original_secret" ::: 'NoDef :=> 'Null 'PGtext
         , "owner_nsid" ::: 'NoDef :=> 'NotNull 'PGtext
         , "owner_real_name" ::: 'NoDef :=> 'Null 'PGtext
         , "owner_username" ::: 'NoDef :=> 'NotNull 'PGtext
         , "photo_page_url" ::: 'NoDef :=> 'Null 'PGtext
         ]
    )

setup :: Definition '[] Schema
setup =
  createTable #content
    ( bigserial `as` #id :*
      (text & notNullable) `as` #hash_id :*
      (int4 & notNullable) `as` #type_id :*
      (text & notNullable) `as` #url :*
      (text & notNullable) `as` #state :*
      (timestampWithTimeZone & notNullable & default_ currentTimestamp) `as` #initialized_at :*
      (timestampWithTimeZone & nullable) `as` #active_at :*
      (timestampWithTimeZone & nullable) `as` #completed_at :*
      (text & nullable) `as` #title :*
      (text & nullable) `as` #attribution_text :*
      (text & nullable) `as` #attribution_link :*
      (text & nullable) `as` #mime :*
      (bigint & nullable) `as` #size :*
      (text & nullable) `as` #error :*
      (doublePrecision & notNullable & default_ 0) `as` #progress :*
      (int4 & notNullable & default_ 0) `as` #abuse_level_id :*
      (bigint & notNullable & default_ 0) `as` #num_abuse_reports :*
      (bigint & notNullable & default_ 0) `as` #num_views :*
      (int & notNullable & default_ 4) `as` #version
    )
    ( primaryKey #id `as` #pk_content )
  >>>
  createTable #image
    ( bigserial `as` #content_id :*
      (timestampWithTimeZone & notNullable & default_ currentTimestamp) `as` #created_at :*
      (bigint & notNullable) `as` #width :*
      (bigint & notNullable) `as` #height :*
      (int4 & notNullable) `as` #tile_size :*
      (int4 & notNullable) `as` #tile_overlap :*
      (text & notNullable) `as` #tile_format
    )
    ( primaryKey #content_id `as` #pk_image :*
      foreignKey #content_id #content #id
      OnDeleteCascade OnUpdateCascade `as` #fk_content_id
    )
  >>>
  createTable #flickr
    ( bigserial `as` #content_id :*
      (int4 & notNullable) `as` #farm_id :*
      (int4 & notNullable) `as` #server_id :*
      (text & notNullable) `as` #photo_id :*
      (text & notNullable) `as` #secret :*
      (int4 & notNullable) `as` #size_id :*
      (bool & notNullable) `as` #is_public :*
      (int4 & notNullable) `as` #license_id :*
      (text & nullable) `as` #original_extension :*
      (text & nullable) `as` #original_secret :*
      (text & notNullable) `as` #owner_nsid :*
      (text & nullable) `as` #owner_real_name :*
      (text & notNullable) `as` #owner_username :*
      (text & nullable) `as` #photo_page_url
    )
    ( primaryKey #content_id `as` #pk_flickr :*
      foreignKey #content_id #content #id
      OnDeleteCascade OnUpdateCascade `as` #fk_content_id
    )

teardown :: Definition Schema '[]
teardown = dropTable #flickr >>> dropTable #image >>> dropTable #content
