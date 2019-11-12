{-# OPTIONS_GHC -O0 #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeOperators #-}

module ZoomHub.Storage.PostgreSQL.Schema
    ( Schemas
    -- * Migrations
    , migrations
    ) where

import qualified ZoomHub.Types.ContentState as ContentState
import qualified ZoomHub.Types.ContentType as ContentType

import qualified Control.Category as Category
import Data.String (IsString)
import Squeal.PostgreSQL
  ( (:::)
  , (:=>)
  , AlignedList((:>>), Done)
  , ColumnConstraint(Def, NoDef)
  , Definition
  , Manipulation(UnsafeManipulation)
  , NP((:*))
  , NullityType(NotNull, Null)
  , OnDeleteClause(OnDeleteCascade)
  , OnUpdateClause(OnUpdateCascade)
  , Optional(Default, Set)
  , PGType(PGbool, PGfloat8, PGint4, PGint8, PGtext, PGtimestamptz)
  , Public
  , SchemumType(Table)
  , TableConstraint(ForeignKey, PrimaryKey, Unique)
  , as
  , bigint
  , bigserial
  , bool
  , createTable
  , currentTimestamp
  , default_
  , deleteFrom_
  , doublePrecision
  , dropTable
  , foreignKey
  , insertInto_
  , int
  , int4
  , literal
  , manipDefinition
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
import Squeal.PostgreSQL.Manipulation
import Squeal.PostgreSQL.Migration (Migration(..))
import Text.RawString.QQ (r)

type Schema =
    '[ ConfigTable
     , ContentTable
     , ImageTable
     , FlickrTable
     ]
type Schemas = Public Schema

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

migrations :: AlignedList (Migration Definition) (Public '[]) Schemas
migrations =
      installPLpgSQLExtension
  :>> initializeHashidsEncode
  :>> initialSchema
  :>> insertHashidsSecret
  :>> createContentHashIdTrigger
  :>> Done

installPLpgSQLExtension :: Migration Definition (Public '[]) (Public '[])
installPLpgSQLExtension = Migration
  { name = "2019-11-11-1: Install V8 extension"
  , up = manipDefinition . UnsafeManipulation $ "CREATE EXTENSION IF NOT EXISTS plpgsql;"
  , down = manipDefinition . UnsafeManipulation $ "DROP EXTENSION IF EXISTS plpgsql;"
  }

initializeHashidsEncode :: Migration Definition (Public '[]) (Public '[])
initializeHashidsEncode = Migration
  { name = "2019-11-11-2: Initialize Hashids encode function"
  , up = concatDefinitions $ manipDefinition . UnsafeManipulation <$> createHashidsFunctions
  , down = manipDefinition . UnsafeManipulation $ dropHashidsEncode
  }
  where
  dropHashidsEncode :: IsString a => a
  dropHashidsEncode = [r|
      DROP SCHEMA hashids CASCADE;
    |]

initialSchema :: Migration Definition (Public '[]) Schemas
initialSchema = Migration
  { name = "2019-11-11-3: Initial setup"
  , up = setup
  , down = teardown
  }
  where
    setup :: Definition (Public '[]) Schemas
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
        defaultContentTypeId = literal ContentType.Unknown
        defaultContentState = literal ContentState.Initialized
        defaultContentVersion = 4

    teardown :: Definition Schemas (Public '[])
    teardown =
      dropTable #flickr >>>
      dropTable #image >>>
      dropTable #content >>>
      dropTable #config

insertHashidsSecret :: Migration Definition Schemas Schemas
insertHashidsSecret = Migration
  { name = "2019-11-11-4: Insert Hashids secret"
  , up = manipDefinition $
      insertInto_ #config (Values_ ( Default `as` #id :* Set "hashids_salt" `as` #key :* Set "secret-salt" `as` #value ))
  , down = manipDefinition $
      deleteFrom_ #config (#key .== "hashids_salt")
  }

createContentHashIdTrigger :: Migration Definition Schemas Schemas
createContentHashIdTrigger = Migration
  { name = "2019-11-11-5: Create content hash_id trigger"
  , up = concatDefinitions
          [ manipDefinition . UnsafeManipulation $ createContentBeforeInsert
          , manipDefinition . UnsafeManipulation $ createTriggerContentBeforeInsert
          ]
  , down = concatDefinitions
            [ manipDefinition . UnsafeManipulation $ dropTriggerContentBeforeInsert
            , manipDefinition . UnsafeManipulation $ dropContentBeforeInsert
            ]
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
                new_hash_id := hashids.encode(
                  number := current_id,
                  min_length := hashids_min_length,
                  salt := hashids_secret_salt
                );
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

-- NOTE: Could we add a `Monoid Definition` so we could use `mconcat` instead
-- of categories? This may be more beginner friendly.
concatDefinitions :: [Definition schemas schemas] -> Definition schemas schemas
concatDefinitions = foldr (>>>) Category.id

-- NOTE: List these values at the bottom as it messes up syntax highlighting of
-- subsequent code in VS Code :(
--
-- hashids.sql
-- Source: https://github.com/andreystepanov/hashids.sql
-- License:
--  MIT License
--  Copyright (c) 2018 Andrey Stepanov
createHashidsFunctions :: IsString a => [a]
createHashidsFunctions =
  [ [r|
      create schema if not exists hashids;
    |]
  , [r|
      create or replace function hashids.to_alphabet(
        number bigint,
        alphabet varchar
      ) returns text as $$
      declare
        id text := '';
        current_number bigint := number;
        alphabet_arr varchar [] := regexp_split_to_array( alphabet, '' );
        alphabet_length integer := length( alphabet );
      begin
        while current_number > 0 loop
          id := alphabet_arr [( current_number % alphabet_length ) + 1] || id;
          current_number := current_number / alphabet_length;
        end loop;

        return id;
      end;
      $$ language plpgsql;
    |]
  , [r|
      create or replace function hashids.from_alphabet(
        id varchar,
        alphabet varchar,
        out number bigint
      ) as $$
      declare
        alphabet_arr varchar [] := regexp_split_to_array( alphabet, '' );
        parts varchar [] := regexp_split_to_array( id, '' );
        parts_length integer := array_length( parts, 1 );
        letter varchar;
        letter_position integer;
      begin
        number := 0;

        for i in 1..parts_length loop
          letter := parts [i];
          letter_position := array_position( alphabet_arr, letter ) - 1;

          number := number * length( alphabet ) + letter_position;
        end loop;
      end;
      $$ language plpgsql;
    |]
  , [r|
      create or replace function hashids.shuffle(
        alphabet varchar = '',
        salt varchar = ''
      ) returns varchar as $$
      declare
        alphabet_arr varchar [] := regexp_split_to_array( alphabet, '' );
        alphabet_length integer := length( alphabet );
        char_position integer := 1;
        shuffle_v integer := 0;
        shuffle_p integer := 0;
        shuffle_j integer := 0;
        shuffle_integer integer := 0;
        shuffle_tmp varchar;
        salt_char varchar;
        old_position integer;
        new_position integer;
      begin
        if length( salt ) < 1 then
          return alphabet;
        end if;

        for i in reverse ( alphabet_length - 1 )..1 loop
          shuffle_v := shuffle_v % length( salt );
          char_position := shuffle_v + 1;

          salt_char := substr( salt, char_position, 1 );
          shuffle_integer := ascii( salt_char );
          shuffle_p = shuffle_p + shuffle_integer;
          shuffle_j = ( shuffle_integer + shuffle_v + shuffle_p ) % i;

          old_position = shuffle_j + 1;
          new_position = i + 1;

          shuffle_tmp = alphabet_arr [new_position];

          alphabet_arr [new_position] = alphabet_arr [old_position];
          alphabet_arr [old_position] = shuffle_tmp;
          shuffle_v := ( shuffle_v + 1 );
        end loop;

        return array_to_string( alphabet_arr, '' );
      end;
      $$ language plpgsql;
    |]
  , [r|
      create or replace function hashids.split( id varchar = '', separators varchar = '', out parts varchar [] ) as $$
      begin
        if length( separators ) < 1 then
          parts := '{}' :: varchar [];
        else
          parts := regexp_split_to_array( regexp_replace( id, '[' || separators || ']', ' ', 'g' ), ' ' );
        end if;
      end;
      $$ language plpgsql;
    |]
  , [r|
      create or replace function hashids.unique_alphabet( in alphabet varchar = '', separators varchar = '', out new_alphabet varchar [] ) as $$
      declare
        alphabet_arr varchar [] := regexp_split_to_array( alphabet, '' );
        separators_arr varchar [] := regexp_split_to_array( separators, '' );
        letter varchar;
      begin
        new_alphabet := '{}' :: varchar [];

        for i in 1..array_length( alphabet_arr, 1 ) loop
          letter := alphabet_arr [i];

          if (
            array_position( new_alphabet, letter ) is not null or
            array_position( separators_arr, letter ) is not null
          ) then
            continue;
          end if;

          new_alphabet := array_append( new_alphabet, letter );
        end loop;
      end;
      $$ language plpgsql;
    |]
  , [r|
      create or replace function hashids._prepare(
        inout alphabet varchar,
        in salt varchar = '',
        out alphabet_arr varchar [],
        out alphabet_length integer,
        out original_alphabet varchar,
        out original_alphabet_arr varchar [],
        out separators varchar,
        out separators_arr varchar [],
        out separators_length integer,
        out guards varchar,
        out guards_length integer
      ) as $$
      declare
        min_alphabet_length integer := 16;
        sep_div integer := 3.5;
        guard_div integer := 12;
        guard_count integer;
        cur_sep varchar;
        diff varchar;
      begin
        if alphabet is null then
          alphabet := 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890';
        end if;

        original_alphabet := alphabet;
        original_alphabet_arr := regexp_split_to_array( alphabet, '' );
        guards := '';
        separators := 'cfhistuCFHISTU';
        separators_arr := regexp_split_to_array( separators, '' );
        alphabet_arr := hashids.unique_alphabet( alphabet := alphabet, separators := separators );

        alphabet := array_to_string( alphabet_arr, '' );
        alphabet_length := array_length( alphabet_arr, 1 );

        if alphabet_length < min_alphabet_length then
          raise exception '[hash_id] Alphabet must contain at least % unique characters', min_alphabet_length;
        end if;

        if array_position( alphabet_arr, ' ' ) is not null then
          raise exception '[hash_id] error: Alphabet cannot contain spaces';
        end if;

        for i in 1..length( separators ) loop
          cur_sep := array_position( original_alphabet_arr, separators_arr [i] );

          if cur_sep is null then
            separators := substr( separators, 1, i ) || ' ' || substr( separators, i + 1 );
          end if;
        end loop;

        separators := regexp_replace( separators, '[ ]', '', 'g' );
        separators := hashids.shuffle( separators, salt );

        if ( length( separators ) < 1 or ( length( alphabet ) / length( separators ) ) > sep_div ) then
          separators_length = ceil( length( alphabet ) / sep_div );

          if ( separators_length > length( separators ) ) then
            diff := separators_length - length( separators );
            separators := separators || substr( alphabet, 1, diff );
            alphabet := substr( alphabet, diff );
          end if;
        end if;

        alphabet := hashids.shuffle( alphabet, salt );
        guard_count := ceil( length( alphabet ) / guard_div );

        if length( alphabet ) < 3 then
          guards := substr( separators, 1, guard_count );
          separators := substr( separators, guard_count );
        else
          guards := substr( alphabet, 1, guard_count );
          alphabet := substr( alphabet, guard_count + 1 );
        end if;

        alphabet_arr := regexp_split_to_array( alphabet, '' );
        alphabet_length := length( alphabet );
        separators_length := length( separators );
        guards_length := length( guards );
      end;
      $$ language plpgsql;
    |]
  , [r|
      create or replace function hashids.encode(
        number anyelement,
        salt varchar = '',
        min_length integer = null,
        alphabet varchar = null
      ) returns text as $$
      declare
        optns record;
        alphabet_arr varchar [];
        alphabet_length int;
        separators_length int;
        guards_length int;
        guard_index integer;
        guards varchar;
        guard varchar = '';
        separators varchar;
        i integer := 0;
        hash_id text := '';
        numbers_id_int bigint := 0;
        numbers bigint [];
        numbers_length integer;
        current_num bigint;
        lottery varchar := '';
        half_length integer;
        excess integer;
        buffer text := '';
        last_id text;
      begin
        optns := hashids._prepare( salt := salt, alphabet := alphabet );
        alphabet := optns.alphabet;
        alphabet_arr := optns.alphabet_arr;
        alphabet_length := optns.alphabet_length;
        separators := optns.separators;
        separators_length := optns.separators_length;
        guards := optns.guards;
        guards_length := optns.guards_length;

        if min_length is null then
          min_length := 0;
        end if;

        if number :: text ~ '^\{.*\}$' then -- if number parameter is an array
          numbers := number;
        else
          numbers := array [number];
        end if;

        numbers_length := array_length( numbers, 1 );

        if numbers_length = 0 then
          return hash_id;
        end if;

        for i in 0..numbers_length - 1 loop
          numbers_id_int := numbers_id_int + ( numbers [i + 1] % ( i + 100 ) );
        end loop;

        hash_id := alphabet_arr [( numbers_id_int % alphabet_length ) + 1];
        lottery := hash_id;

        for i in 0..numbers_length - 1 loop
          current_num := numbers [i + 1];
          buffer := lottery || salt || alphabet;

          alphabet := hashids.shuffle( alphabet, substr( buffer, 1, alphabet_length ) );
          last_id := hashids.to_alphabet( current_num, alphabet );

          hash_id := hash_id || last_id;

          if ( i < numbers_length - 1 ) then
            current_num = current_num % ascii( substr( last_id, 1, 1 ) ) + i;
            hash_id := hash_id || substr( separators, ( current_num % separators_length ) :: integer + 1, 1 );
          end if;
        end loop;

        if length( hash_id ) < min_length then
          guard_index := ( numbers_id_int + ascii( substr( hash_id, 1, 1 ) ) ) % guards_length;
          guard := substr( guards, guard_index + 1, 1 );

          hash_id = guard || hash_id;

          if length( hash_id ) < min_length then
            guard_index := ( numbers_id_int + ascii( substr( hash_id, 3, 1 ) ) ) % guards_length;
            guard := substr( guards, guard_index + 1, 1 );

            hash_id := hash_id || guard;
          end if;
        end if;

        half_length = ( length( alphabet ) / 2 );

        while ( length( hash_id ) < min_length ) loop
          alphabet := hashids.shuffle( alphabet, alphabet );
          hash_id := substr( alphabet, half_length + 1 ) || hash_id || substr( alphabet, 1, half_length );

          excess := length( hash_id ) - min_length;

          if excess > 0 then
            hash_id := substr( hash_id, ( excess / 2 ) + 1, min_length );
          end if;
        end loop;

        return hash_id;
      end;
      $$ language plpgsql;
    |]
  , [r|
      create or replace function hashids.decode(
        in id varchar,
        in alphabet varchar = null,
        in salt varchar = '',
        in min_length integer = null
      ) returns bigint [] as $$
      declare
        optns record;
        numbers bigint [];
        empty_array bigint [];
        parts varchar [];
        parts_count integer;
        id_breakdown varchar;
        lottery varchar;
        sub_id varchar;
        buffer varchar;
        idx integer := 1;
      begin
        numbers := array [] :: bigint [];
        empty_array := numbers;

        if ( id is null or length( id ) = 0 ) then
          return empty_array;
        end if;

        optns := hashids._prepare( salt := salt, alphabet := alphabet );
        alphabet := optns.alphabet;
        parts := hashids.split( id, optns.guards );
        parts_count = array_length( parts, 1 );

        if parts_count = 3 or parts_count = 2 then
          idx := 2;
        end if;

        id_breakdown := parts [idx];

        lottery := substr( id_breakdown, 1, 1 );
        id_breakdown := substr( id_breakdown, 2 );

        parts := hashids.split( id_breakdown, optns.separators );
        parts_count = array_length( parts, 1 );

        for i in 1..parts_count loop
          sub_id := parts [i];
          buffer := lottery || salt || alphabet;

          alphabet := hashids.shuffle( alphabet, substr( buffer, 1, optns.alphabet_length ) );
          numbers := numbers || hashids.from_alphabet( sub_id, alphabet );
        end loop;

        if (
          array_length( numbers, 1 ) = 0 or
          hashids.encode(
              number := numbers,
              alphabet := optns.original_alphabet,
              salt := salt,
              min_length := min_length
          ) is distinct from id
        ) then
          return empty_array;
        end if;

        return numbers;
      end;
      $$ language plpgsql;
    |]
  , [r|
      create or replace function hashids.encode_hex(
        hex varchar,
        salt varchar = '',
        min_length integer = null,
        alphabet varchar = null
      ) returns varchar as $$
      declare
        parts varchar [];
        numbers bigint [];
        number bigint;
      begin
        if not hex ~ '^[0-9a-fA-F]+$' then
          return '';
        end if;

        execute 'select array(select t[1] from regexp_matches( $1, ''[\w\\W]{1,12}'', ''g'') t)'
        into parts
        using hex;

        for i in 1..array_length( parts, 1 ) loop
          number := ( 'x' || lpad( ( '1' || parts [i] ), 16, '0' ) ) :: bit( 64 ) :: bigint;
          numbers := array_append( numbers, number );
        end loop;

        return hashids.encode( number := numbers, salt := salt, min_length := min_length, alphabet := alphabet );
      end;
      $$ language plpgsql;
    |]
  , [r|
      create or replace function hashids.decode_hex(
        id varchar,
        salt varchar = '',
        min_length integer = null,
        alphabet varchar = null
      ) returns varchar as $$
      declare
        hex varchar = '';
        numbers bigint [];
      begin
        numbers := hashids.decode( id := id, salt := salt, min_length := min_length, alphabet := alphabet );

        for i in 1..array_length( numbers, 1 ) loop
          hex := hex || substr( to_hex( numbers [i] ), 2 );
        end loop;

        return hex;
      end;
      $$ language plpgsql;
    |]
  ]
