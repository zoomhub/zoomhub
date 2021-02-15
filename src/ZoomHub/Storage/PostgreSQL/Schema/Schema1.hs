{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -O0 #-}

module ZoomHub.Storage.PostgreSQL.Schema.Schema1
  ( ContentTable1,
  )
where

import Squeal.PostgreSQL
  ( (:::),
    (:=>),
    ColumnConstraint (Def, NoDef),
    NullityType (NotNull, Null),
    PGType (PGfloat8, PGint4, PGint8, PGtext, PGtimestamptz),
    SchemumType (Table),
    TableConstraint (PrimaryKey, Unique),
  )

type ContentTable1 =
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
                     "submitter_email" ::: 'NoDef :=> 'Null 'PGtext
                   ]
          )
