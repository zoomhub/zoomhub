{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module ZoomHub.Storage.PostgreSQL2
  ( -- ** Read operations
    getById

  , Schema
  , setup
  , getContent
  , run
  ) where

import ZoomHub.Storage.PostgreSQL.Internal (createConnectionPool)
import ZoomHub.Types.Content (Content)
import ZoomHub.Types.ContentId (ContentId)

import Control.Monad (void)
import Control.Monad.Base (liftBase)
import Data.Int (Int64)
import Data.Text (Text)
import qualified GHC.Generics as GHC
import qualified Generics.SOP as SOP
import Squeal.PostgreSQL -- (Table, PrimaryKey, NotNull, PGint8)
import Squeal.PostgreSQL.Render

-- Public API

-- Schema
type Schema =
    '[ "content" ::: 'Table (
         '[ "pk_content" ::: 'PrimaryKey '["id"] ] :=>
         '[ "id" ::: 'Def :=> 'NotNull 'PGint8
          , "hash_id" ::: 'NoDef :=> 'NotNull 'PGtext
          , "type_id" ::: 'NoDef :=> 'NotNull 'PGint8
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
          , "abuse_level_id" ::: 'Def :=> 'NotNull 'PGint8
          , "num_abuse_reports" ::: 'Def :=> 'NotNull 'PGint8
          , "num_views" ::: 'Def :=> 'NotNull 'PGint8
          , "version" ::: 'Def :=> 'NotNull 'PGint4
          ])
     , "image" ::: 'Table (
         '[ "pk_image" ::: 'PrimaryKey '["content_id"]
          , "fk_content_id" ::: 'ForeignKey '["content_id"] "content" '["id"]
          ] :=>
         '[ "content_id" ::: 'Def :=> 'NotNull 'PGint8
          , "initialized_at" ::: 'Def :=> 'NotNull 'PGtimestamptz
          , "width" ::: 'NoDef :=> 'NotNull 'PGint8
          , "height" ::: 'NoDef :=> 'NotNull 'PGint8
          , "tile_size" ::: 'NoDef :=> 'NotNull 'PGint4
          , "tile_overlap" ::: 'NoDef :=> 'NotNull 'PGint4
          , "tile_format" ::: 'NoDef :=> 'NotNull 'PGtext
          ])
     , "flickr" ::: 'Table (
         '[ "pk_flickr" ::: 'PrimaryKey '["content_id"]
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
          ])
     ]


setup :: Definition '[] Schema
setup =
  createTable #content
    ( bigserial `as` #id :*
      (text & notNullable) `as` #hash_id :*
      (bigint & notNullable) `as` #type_id :*
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
      (bigint & notNullable & default_ 0) `as` #abuse_level_id :*
      (bigint & notNullable & default_ 0) `as` #num_abuse_reports :*
      (bigint & notNullable & default_ 0) `as` #num_views :*
      (int & notNullable & default_ 0) `as` #version
    )
    ( primaryKey #id `as` #pk_content )
  >>>
  createTable #image
    ( bigserial `as` #content_id :*
      (timestampWithTimeZone & notNullable & default_ currentTimestamp) `as` #initialized_at :*
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

-- Reads
getById :: ContentId -> Connection -> IO (Maybe Content)
getById = undefined

getContent :: Query Schema '[]
  '[ "contentId"  ::: 'NotNull 'PGint8
   , "contentHashId" ::: 'NotNull 'PGtext
   , "contentWidth" ::: 'Null 'PGint8
   ]
getContent = select
  (#c ! #id `as` #contentId :*
   #c ! #hash_id `as` #contentHashId :*
   #i ! #width `as` #contentWidth
  )
  ( from (table (#content `as` #c)
    & leftOuterJoin (table (#image `as` #i))
      (#c ! #id .== #i ! #content_id)) & limit 10 )

data ContentRow = ContentRow
    { contentId :: Int64
    , contentHashId :: Text
    , contentWidth :: Maybe Int64
    } deriving (Show, GHC.Generic)
instance SOP.Generic ContentRow
instance SOP.HasDatatypeInfo ContentRow

run :: IO ()
run = let
  session :: PQ Schema Schema IO ()
  session = do
    contentResult <- runQuery getContent
    contentRows <- getRows contentResult
    liftBase $ print (contentRows :: [ContentRow])
  in
    void . withConnection "host=localhost port=5432 dbname=zoomhub_development" $ session
