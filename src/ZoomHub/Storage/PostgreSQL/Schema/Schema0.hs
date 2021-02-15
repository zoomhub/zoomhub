{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -O0 #-}

module ZoomHub.Storage.PostgreSQL.Schema.Schema0
  ( ConfigTable0,
    ContentTable0,
    ImageTable0,
    FlickrTable0,
  )
where

import Squeal.PostgreSQL
  ( (:::),
    (:=>),
    ColumnConstraint (Def, NoDef),
    NullityType (NotNull, Null),
    PGType (PGbool, PGfloat8, PGint4, PGint8, PGtext, PGtimestamptz),
    SchemumType (Table),
    TableConstraint (ForeignKey, PrimaryKey, Unique),
  )

type ConfigTable0 =
  "config"
    ::: 'Table
          ( '[ "pk_config" ::: 'PrimaryKey '["id"],
               "config_unique_key" ::: 'Unique '["key"]
             ]
              :=> '[ "id" ::: 'Def :=> 'NotNull 'PGint8,
                     "key" ::: 'NoDef :=> 'NotNull 'PGtext,
                     "value" ::: 'NoDef :=> 'NotNull 'PGtext
                   ]
          )

type ContentTable0 =
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
                     "version" ::: 'Def :=> 'NotNull 'PGint4
                   ]
          )

type ImageTable0 =
  "image"
    ::: 'Table
          ( '[ "pk_image" ::: 'PrimaryKey '["content_id"],
               "fk_content_id" ::: 'ForeignKey '["content_id"] "content" '["id"],
               "image_unique_content_id" ::: 'Unique '["content_id"]
             ]
              :=> '[ "content_id" ::: 'Def :=> 'NotNull 'PGint8,
                     "created_at" ::: 'Def :=> 'NotNull 'PGtimestamptz,
                     "width" ::: 'NoDef :=> 'NotNull 'PGint8,
                     "height" ::: 'NoDef :=> 'NotNull 'PGint8,
                     "tile_size" ::: 'NoDef :=> 'NotNull 'PGint4,
                     "tile_overlap" ::: 'NoDef :=> 'NotNull 'PGint4,
                     "tile_format" ::: 'NoDef :=> 'NotNull 'PGtext
                   ]
          )

type FlickrTable0 =
  "flickr"
    ::: 'Table
          ( '[ "pk_flickr" ::: 'PrimaryKey '["content_id"],
               "fk_content_id" ::: 'ForeignKey '["content_id"] "content" '["id"],
               "flickr_unique_content_id" ::: 'Unique '["content_id"]
             ]
              :=> '[ "content_id" ::: 'Def :=> 'NotNull 'PGint8,
                     "farm_id" ::: 'NoDef :=> 'NotNull 'PGint4,
                     "server_id" ::: 'NoDef :=> 'NotNull 'PGint4,
                     "photo_id" ::: 'NoDef :=> 'NotNull 'PGtext,
                     "secret" ::: 'NoDef :=> 'NotNull 'PGtext,
                     "size_id" ::: 'NoDef :=> 'NotNull 'PGint4,
                     "is_public" ::: 'NoDef :=> 'NotNull 'PGbool,
                     "license_id" ::: 'NoDef :=> 'NotNull 'PGint4,
                     "original_extension" ::: 'NoDef :=> 'Null 'PGtext,
                     "original_secret" ::: 'NoDef :=> 'Null 'PGtext,
                     "owner_nsid" ::: 'NoDef :=> 'NotNull 'PGtext,
                     "owner_real_name" ::: 'NoDef :=> 'Null 'PGtext,
                     "owner_username" ::: 'NoDef :=> 'NotNull 'PGtext,
                     "photo_page_url" ::: 'NoDef :=> 'Null 'PGtext
                   ]
          )
