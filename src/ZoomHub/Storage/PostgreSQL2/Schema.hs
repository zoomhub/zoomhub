{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module ZoomHub.Storage.PostgreSQL2.Schema
    ( Schema
    -- * Migrations
    , migrations
    ) where

import qualified ZoomHub.Types.ContentState as ContentState
import qualified ZoomHub.Types.ContentType as ContentType

import Control.Monad (void)
import Data.String (IsString)
import Squeal.PostgreSQL
  ( (:::)
  , (:=>)
  , AlignedList((:>>), Done)
  , ColumnConstraint(Def, NoDef)
  , ColumnValue(Default, Set)
  , Definition
  , Manipulation(UnsafeManipulation)
  , NP((:*))
  , NullityType(NotNull, Null)
  , OnDeleteClause(OnDeleteCascade)
  , OnUpdateClause(OnUpdateCascade)
  , PGType(PGbool, PGfloat8, PGint4, PGint8, PGtext, PGtimestamptz)
  , SchemumType(Table)
  , TableConstraint(ForeignKey, PrimaryKey, Unique)
  , as
  , bigint
  , bigserial
  , bool
  , createTable
  , currentTimestamp
  , default_
  , define
  , deleteFrom_
  , doublePrecision
  , dropTable
  , foreignKey
  , insertRow_
  , int
  , int4
  , manipulate
  , notNullable
  , null_
  , nullable
  , primaryKey
  , text
  , timestampWithTimeZone
  , unique
  , (&)
  , (.==)
  , (>>>)
  )
import Squeal.PostgreSQL.Migration (Migration(..), MigrationsTable)
import Text.RawString.QQ (r)

type Schema' =
    '[ ConfigTable
     , ContentTable
     , ImageTable
     , FlickrTable
     ]

type Schema = ("schema_migrations" ::: 'Table MigrationsTable) : Schema'

type ConfigTable =
  "config" ::: 'Table
    ( '[ "pk_config" ::: 'PrimaryKey '["id"]
       , "config_unique_key" ::: 'Unique '["key"]
       ] :=>
        '[ "id" ::: 'Def :=> 'NotNull 'PGint8
         , "key" ::: 'NoDef :=> 'NotNull 'PGtext
         , "value" ::: 'NoDef :=> 'NotNull 'PGtext
         ]
    )

type ContentTable =
  "content" ::: 'Table
    ( '[ "pk_content" ::: 'PrimaryKey '["id"]
       , "content_unique_hash_id" ::: 'Unique '["hash_id"]
       , "content_unique_url" ::: 'Unique '["url"]
       ] :=>
        '[ "id" ::: 'Def :=> 'NotNull 'PGint8
         , "hash_id" ::: 'NoDef :=> 'NotNull 'PGtext
         , "type_id" ::: 'Def :=> 'NotNull 'PGint4
         , "url" ::: 'NoDef :=> 'NotNull 'PGtext
         , "state" ::: 'Def :=> 'NotNull 'PGtext
         , "initialized_at" ::: 'Def :=> 'NotNull 'PGtimestamptz
         , "active_at" ::: 'Def :=> 'Null 'PGtimestamptz
         , "completed_at" ::: 'Def :=> 'Null 'PGtimestamptz
         , "title" ::: 'Def :=> 'Null 'PGtext
         , "attribution_text" ::: 'Def :=> 'Null 'PGtext
         , "attribution_link" ::: 'Def :=> 'Null 'PGtext
         , "mime" ::: 'Def :=> 'Null 'PGtext
         , "size" ::: 'Def :=> 'Null 'PGint8
         , "error" ::: 'Def :=> 'Null 'PGtext
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
        , "image_unique_content_id" ::: 'Unique '["content_id"]
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
        , "flickr_unique_content_id" ::: 'Unique '["content_id"]
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

migrations :: AlignedList (Migration IO) '[] Schema'
migrations =
      installV8Extension
  :>> initializeHashidsEncode
  :>> initialSchema
  :>> insertHashidsSecret
  :>> createContentHashIdTrigger
  :>> Done

installV8Extension :: Migration IO '[] '[]
installV8Extension = Migration
  { name = "Install V8 extension"
  , up = void . manipulate . UnsafeManipulation $ "CREATE EXTENSION plv8;"
  , down = void . manipulate . UnsafeManipulation $ "DROP EXTENSION plv8;"
  }

initializeHashidsEncode :: Migration IO '[] '[]
initializeHashidsEncode = Migration
  { name = "Initialize Hashids encode function"
  , up = do
      void . manipulate . UnsafeManipulation $ createLoadHashids
      void . manipulate . UnsafeManipulation $ createHashidsEncode
      void . manipulate . UnsafeManipulation $ "SET plv8.start_proc = 'load_hashids';"
  , down = do
      void . manipulate . UnsafeManipulation $ "RESET plv8.start_proc;"
      void . manipulate . UnsafeManipulation $ dropHashidsEncode
      void . manipulate . UnsafeManipulation $ dropLoadHashids
  }
  where
  dropLoadHashids :: IsString a => a
  dropLoadHashids = [r|
      DROP FUNCTION load_hashids();
    |]

  createHashidsEncode :: IsString a => a
  createHashidsEncode = [r|
      CREATE FUNCTION hashids_encode(salt TEXT, min_length BIGINT, key BIGINT) RETURNS TEXT AS $$
          return new Hashids(salt, min_length).encode(key);
      $$ LANGUAGE PLV8 IMMUTABLE STRICT;
    |]

  dropHashidsEncode :: IsString a => a
  dropHashidsEncode = [r|
      DROP FUNCTION hashids_encode(salt TEXT, min_length BIGINT, key BIGINT);
    |]

createContentHashIdTrigger :: Migration IO Schema' Schema'
createContentHashIdTrigger = Migration
  { name = "Create content hash_id trigger"
  , up = do
      void . manipulate . UnsafeManipulation $ createContentBeforeInsert
      void . manipulate . UnsafeManipulation $ createTriggerContentBeforeInsert
  , down = do
      void . manipulate . UnsafeManipulation $ dropTriggerContentBeforeInsert
      void . manipulate . UnsafeManipulation $ dropContentBeforeInsert
  }
  where
  createContentBeforeInsert :: IsString a => a
  createContentBeforeInsert = [r|
      CREATE FUNCTION content_before_insert() RETURNS trigger AS $$
          DECLARE
            iterations INT := 0;
            max_iterations INT := 100;
            current_id INT := NEW.id;
            select_by_hash_id_query TEXT = 'SELECT id FROM ' || quote_ident(TG_TABLE_NAME) || ' WHERE hash_id=';
            found TEXT;
            new_hash_id TEXT;
            hashids_min_length INT := 3;
            hashids_secret_salt TEXT;
          BEGIN
              hashids_secret_salt := (SELECT value FROM config WHERE key='hashids_salt') ;

              LOOP
                new_hash_id := hashids_encode(hashids_secret_salt, hashids_min_length, current_id);
                EXECUTE select_by_hash_id_query || quote_literal(new_hash_id) INTO found;

                IF found IS NULL THEN
                  EXIT;
                END IF;

                IF iterations > max_iterations THEN
                  RAISE EXCEPTION
                    'Too many iterations to find new hash ID. Max: %, current: %.',
                    max_iterations, iterations
                  USING HINT = 'Check content table for hash ID collisions';
                END IF;

                iterations := iterations + 1;
                current_id := current_id + 1;
              END LOOP;

              NEW.title = NEW.title || '-' || iterations;
              NEW.hash_id := new_hash_id;
              RETURN NEW;
          END;
      $$ LANGUAGE plpgsql;
    |]

  dropContentBeforeInsert :: IsString a => a
  dropContentBeforeInsert = [r|
      DROP FUNCTION content_before_insert();
    |]

  createTriggerContentBeforeInsert :: IsString a => a
  createTriggerContentBeforeInsert = [r|
      CREATE TRIGGER content_before_insert BEFORE INSERT ON content
        FOR EACH ROW EXECUTE PROCEDURE content_before_insert();
    |]

  dropTriggerContentBeforeInsert :: IsString a => a
  dropTriggerContentBeforeInsert = [r|
      DROP TRIGGER content_before_insert ON content;
    |]

initialSchema :: Migration IO '[] Schema'
initialSchema = Migration
  { name = "Initial setup"
  , up = void . define $ setup
  , down = void . define $ teardown
  }
  where
    setup :: Definition '[] Schema'
    setup =
      createTable #config
        ( bigserial `as` #id :*
          (text & notNullable) `as` #key :*
          (text & notNullable) `as` #value
        )
        ( primaryKey #id `as` #pk_config :*
          unique #key `as` #config_unique_key
        )
      >>>
      createTable #content
        ( bigserial `as` #id :*
          (text & notNullable) `as` #hash_id :*
          (int4 & notNullable & default_ defaultContentTypeId) `as` #type_id :*
          (text & notNullable) `as` #url :* (text & notNullable & default_ defaultContentState) `as` #state :*
          (timestampWithTimeZone & notNullable & default_ currentTimestamp) `as` #initialized_at :*
          (timestampWithTimeZone & nullable & default_ null_) `as` #active_at :*
          (timestampWithTimeZone & nullable & default_ null_) `as` #completed_at :*
          (text & nullable & default_ null_) `as` #title :*
          (text & nullable & default_ null_) `as` #attribution_text :*
          (text & nullable & default_ null_) `as` #attribution_link :*
          (text & nullable & default_ null_) `as` #mime :*
          (bigint & nullable & default_ null_) `as` #size :*
          (text & nullable & default_ null_) `as` #error :*
          (doublePrecision & notNullable & default_ 0) `as` #progress :*
          (int4 & notNullable & default_ 0) `as` #abuse_level_id :*
          (bigint & notNullable & default_ 0) `as` #num_abuse_reports :*
          (bigint & notNullable & default_ 0) `as` #num_views :*
          (int & notNullable & default_ defaultContentVersion) `as` #version
        )
        ( primaryKey #id `as` #pk_content :*
          unique #hash_id `as` #content_unique_hash_id :*
          unique #url `as` #content_unique_url
        )
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
          ( foreignKey #content_id #content #id
              OnDeleteCascade OnUpdateCascade `as` #fk_content_id ) :*
          unique #content_id `as` #image_unique_content_id
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
          ( foreignKey #content_id #content #id
              OnDeleteCascade OnUpdateCascade `as` #fk_content_id ) :*
          unique #content_id `as` #flickr_unique_content_id
        )
      where
        defaultContentTypeId = ContentType.toExpression ContentType.Unknown
        defaultContentState = ContentState.toExpression ContentState.Initialized
        defaultContentVersion = 4

    teardown :: Definition Schema' '[]
    teardown = dropTable #flickr
      >>> dropTable #image >>> dropTable #content >>> dropTable #config

insertHashidsSecret :: Migration IO Schema' Schema'
insertHashidsSecret = Migration
  { name = "Insert Hashids secret"
  , up = void . manipulate $
      insertRow_ #config
        ( Default `as` #id :*
          Set "hashids_salt" `as` #key :*
          Set "secret-salt" `as` #value
        )
  , down = void . manipulate $
      deleteFrom_ #config (#key .== "hashids_salt")
  }

-- NOTE: List this value at the bottom as it messes up syntax highlighting of
-- subsequent code in VS Code :(
createLoadHashids :: IsString a => a
createLoadHashids = [r|
    CREATE FUNCTION load_hashids() RETURNS VOID
    AS
    $FUNCTION$
    this.Hashids = function Hashids(salt, minLength) {
      this.salt = salt;
      this.minLength = minLength;
      this.encode = function (key) {
        return 'FOOBAR-' + key;
      }
    }
    $FUNCTION$ LANGUAGE PLV8 IMMUTABLE STRICT;
  |]
