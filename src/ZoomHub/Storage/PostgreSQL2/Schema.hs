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
    CREATE FUNCTION load_hashids() RETURNS VOID AS $$
      !function(t,e){if("function"==typeof define&&define.amd)define(["module","exports"],e);else if("undefined"!=typeof exports)e(module,exports);else{var s={exports:{}};e(s,s.exports),t.Hashids=s.exports}}(this,function(t,e){"use strict";function s(t,e){if(!(t instanceof e))throw new TypeError("Cannot call a class as a function")}Object.defineProperty(e,"__esModule",{value:!0});var h=function(){function t(t,e){for(var s=0;s<e.length;s++){var h=e[s];h.enumerable=h.enumerable||!1,h.configurable=!0,"value"in h&&(h.writable=!0),Object.defineProperty(t,h.key,h)}}return function(e,s,h){return s&&t(e.prototype,s),h&&t(e,h),e}}(),r=function(){function t(){var e=arguments.length<=0||void 0===arguments[0]?"":arguments[0],h=arguments.length<=1||void 0===arguments[1]?0:arguments[1],r=arguments.length<=2||void 0===arguments[2]?"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890":arguments[2];s(this,t);var a=16,n=3.5,i=12,l="error: alphabet must contain at least X unique characters",u="error: alphabet cannot contain spaces",p="",o=void 0,f=void 0;this.escapeRegExp=function(t){return t.replace(/[-[\]{}()*+?.,\\^$|#\s]/g,"\\$&")},this.parseInt=function(t,e){return/^(\-|\+)?([0-9]+|Infinity)$/.test(t)?parseInt(t,e):NaN},this.seps="cfhistuCFHISTU",this.minLength=parseInt(h,10)>0?h:0,this.salt="string"==typeof e?e:"","string"==typeof r&&(this.alphabet=r);for(var g=0;g!==this.alphabet.length;g++)p.indexOf(this.alphabet.charAt(g))===-1&&(p+=this.alphabet.charAt(g));if(this.alphabet=p,this.alphabet.length<a)throw l.replace("X",a);if(this.alphabet.search(" ")!==-1)throw u;for(var c=0;c!==this.seps.length;c++){var b=this.alphabet.indexOf(this.seps.charAt(c));b===-1?this.seps=this.seps.substr(0,c)+" "+this.seps.substr(c+1):this.alphabet=this.alphabet.substr(0,b)+" "+this.alphabet.substr(b+1)}this.alphabet=this.alphabet.replace(/ /g,""),this.seps=this.seps.replace(/ /g,""),this.seps=this._shuffle(this.seps,this.salt),(!this.seps.length||this.alphabet.length/this.seps.length>n)&&(o=Math.ceil(this.alphabet.length/n),o>this.seps.length&&(f=o-this.seps.length,this.seps+=this.alphabet.substr(0,f),this.alphabet=this.alphabet.substr(f))),this.alphabet=this._shuffle(this.alphabet,this.salt);var d=Math.ceil(this.alphabet.length/i);this.alphabet.length<3?(this.guards=this.seps.substr(0,d),this.seps=this.seps.substr(d)):(this.guards=this.alphabet.substr(0,d),this.alphabet=this.alphabet.substr(d))}return h(t,[{key:"encode",value:function(){for(var t=arguments.length,e=Array(t),s=0;s<t;s++)e[s]=arguments[s];var h="";if(!e.length)return h;if(e[0]&&e[0].constructor===Array&&(e=e[0],!e.length))return h;for(var r=0;r!==e.length;r++)if(e[r]=this.parseInt(e[r],10),!(e[r]>=0))return h;return this._encode(e)}},{key:"decode",value:function(t){var e=[];return t&&t.length&&"string"==typeof t?this._decode(t,this.alphabet):e}},{key:"encodeHex",value:function(t){if(t=t.toString(),!/^[0-9a-fA-F]+$/.test(t))return"";for(var e=t.match(/[\w\W]{1,12}/g),s=0;s!==e.length;s++)e[s]=parseInt("1"+e[s],16);return this.encode.apply(this,e)}},{key:"decodeHex",value:function(t){for(var e=[],s=this.decode(t),h=0;h!==s.length;h++)e+=s[h].toString(16).substr(1);return e}},{key:"_encode",value:function(t){for(var e=void 0,s=this.alphabet,h=0,r=0;r!==t.length;r++)h+=t[r]%(r+100);e=s.charAt(h%s.length);for(var a=e,n=0;n!==t.length;n++){var i=t[n],l=a+this.salt+s;s=this._shuffle(s,l.substr(0,s.length));var u=this._toAlphabet(i,s);if(e+=u,n+1<t.length){i%=u.charCodeAt(0)+n;var p=i%this.seps.length;e+=this.seps.charAt(p)}}if(e.length<this.minLength){var o=(h+e[0].charCodeAt(0))%this.guards.length,f=this.guards[o];e=f+e,e.length<this.minLength&&(o=(h+e[2].charCodeAt(0))%this.guards.length,f=this.guards[o],e+=f)}for(var g=parseInt(s.length/2,10);e.length<this.minLength;){s=this._shuffle(s,s),e=s.substr(g)+e+s.substr(0,g);var c=e.length-this.minLength;c>0&&(e=e.substr(c/2,this.minLength))}return e}},{key:"_decode",value:function(t,e){var s=[],h=0,r=new RegExp("["+this.escapeRegExp(this.guards)+"]","g"),a=t.replace(r," "),n=a.split(" ");if(3!==n.length&&2!==n.length||(h=1),a=n[h],"undefined"!=typeof a[0]){var i=a[0];a=a.substr(1),r=new RegExp("["+this.escapeRegExp(this.seps)+"]","g"),a=a.replace(r," "),n=a.split(" ");for(var l=0;l!==n.length;l++){var u=n[l],p=i+this.salt+e;e=this._shuffle(e,p.substr(0,e.length)),s.push(this._fromAlphabet(u,e))}this._encode(s)!==t&&(s=[])}return s}},{key:"_shuffle",value:function(t,e){var s=void 0;if(!e.length)return t;for(var h=t.length-1,r=0,a=0,n=0;h>0;h--,r++){r%=e.length,a+=s=e.charAt(r).charCodeAt(0),n=(s+r+a)%h;var i=t[n];t=t.substr(0,n)+t.charAt(h)+t.substr(n+1),t=t.substr(0,h)+i+t.substr(h+1)}return t}},{key:"_toAlphabet",value:function(t,e){var s="";do s=e.charAt(t%e.length)+s,t=parseInt(t/e.length,10);while(t);return s}},{key:"_fromAlphabet",value:function(t,e){for(var s=0,h=0;h<t.length;h++){var r=e.indexOf(t[h]);s+=r*Math.pow(e.length,t.length-h-1)}return s}}]),t}();e.default=r,t.exports=e.default});
    $$ LANGUAGE plv8 IMMUTABLE STRICT;
  |]
