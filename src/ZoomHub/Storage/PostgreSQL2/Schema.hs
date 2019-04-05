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
    // Source: https://raw.githubusercontent.com/ivanakimov/hashids.js/1.2.1/dist/index.js
    (function (global, factory) {
      if (typeof define === "function" && define.amd) {
        define(["exports"], factory);
      } else if (typeof exports !== "undefined") {
        factory(exports);
      } else {
        var mod = {
          exports: {}
        };
        factory(mod.exports);
        global.Hashids = mod.exports;
      }
    })(this, function (_exports) {
      "use strict";

      Object.defineProperty(_exports, "__esModule", {
        value: true
      });
      _exports.default = void 0;

      function _classCallCheck(instance, Constructor) { if (!(instance instanceof Constructor)) { throw new TypeError("Cannot call a class as a function"); } }

      function _defineProperties(target, props) { for (var i = 0; i < props.length; i++) { var descriptor = props[i]; descriptor.enumerable = descriptor.enumerable || false; descriptor.configurable = true; if ("value" in descriptor) descriptor.writable = true; Object.defineProperty(target, descriptor.key, descriptor); } }

      function _createClass(Constructor, protoProps, staticProps) { if (protoProps) _defineProperties(Constructor.prototype, protoProps); if (staticProps) _defineProperties(Constructor, staticProps); return Constructor; }

      var Hashids =
      /*#__PURE__*/
      function () {
        function Hashids() {
          var salt = arguments.length > 0 && arguments[0] !== undefined ? arguments[0] : '';
          var minLength = arguments.length > 1 && arguments[1] !== undefined ? arguments[1] : 0;
          var alphabet = arguments.length > 2 && arguments[2] !== undefined ? arguments[2] : 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890';

          _classCallCheck(this, Hashids);

          var minAlphabetLength = 16;
          var sepDiv = 3.5;
          var guardDiv = 12;
          var errorAlphabetLength = 'error: alphabet must contain at least X unique characters';
          var errorAlphabetSpace = 'error: alphabet cannot contain spaces';
          var uniqueAlphabet = '',
              sepsLength,
              diff;
          /* funcs */

          this.escapeRegExp = function (s) {
            return s.replace(/[-[\]{}()*+?.,\\^$|#\s]/g, '\\$&');
          };

          this.parseInt = function (v, radix) {
            return /^(-|\+)?([0-9]+|Infinity)$/.test(v) ? parseInt(v, radix) : NaN;
          };
          /* alphabet vars */


          this.seps = 'cfhistuCFHISTU';
          this.minLength = parseInt(minLength, 10) > 0 ? minLength : 0;
          this.salt = typeof salt === 'string' ? salt : '';

          if (typeof alphabet === 'string') {
            this.alphabet = alphabet;
          }

          for (var i = 0; i !== this.alphabet.length; i++) {
            if (uniqueAlphabet.indexOf(this.alphabet.charAt(i)) === -1) {
              uniqueAlphabet += this.alphabet.charAt(i);
            }
          }

          this.alphabet = uniqueAlphabet;

          if (this.alphabet.length < minAlphabetLength) {
            throw errorAlphabetLength.replace('X', minAlphabetLength);
          }

          if (this.alphabet.search(' ') !== -1) {
            throw errorAlphabetSpace;
          }
          /*
            `this.seps` should contain only characters present in `this.alphabet`
            `this.alphabet` should not contains `this.seps`
          */


          for (var _i = 0; _i !== this.seps.length; _i++) {
            var j = this.alphabet.indexOf(this.seps.charAt(_i));

            if (j === -1) {
              this.seps = this.seps.substr(0, _i) + ' ' + this.seps.substr(_i + 1);
            } else {
              this.alphabet = this.alphabet.substr(0, j) + ' ' + this.alphabet.substr(j + 1);
            }
          }

          this.alphabet = this.alphabet.replace(/ /g, '');
          this.seps = this.seps.replace(/ /g, '');
          this.seps = this._shuffle(this.seps, this.salt);

          if (!this.seps.length || this.alphabet.length / this.seps.length > sepDiv) {
            sepsLength = Math.ceil(this.alphabet.length / sepDiv);

            if (sepsLength > this.seps.length) {
              diff = sepsLength - this.seps.length;
              this.seps += this.alphabet.substr(0, diff);
              this.alphabet = this.alphabet.substr(diff);
            }
          }

          this.alphabet = this._shuffle(this.alphabet, this.salt);
          var guardCount = Math.ceil(this.alphabet.length / guardDiv);

          if (this.alphabet.length < 3) {
            this.guards = this.seps.substr(0, guardCount);
            this.seps = this.seps.substr(guardCount);
          } else {
            this.guards = this.alphabet.substr(0, guardCount);
            this.alphabet = this.alphabet.substr(guardCount);
          }
        }

        _createClass(Hashids, [{
          key: "encode",
          value: function encode() {
            for (var _len = arguments.length, numbers = new Array(_len), _key = 0; _key < _len; _key++) {
              numbers[_key] = arguments[_key];
            }

            var ret = '';

            if (!numbers.length) {
              return ret;
            }

            if (numbers[0] && numbers[0].constructor === Array) {
              numbers = numbers[0];

              if (!numbers.length) {
                return ret;
              }
            }

            for (var i = 0; i !== numbers.length; i++) {
              numbers[i] = this.parseInt(numbers[i], 10);

              if (numbers[i] >= 0) {
                continue;
              } else {
                return ret;
              }
            }

            return this._encode(numbers);
          }
        }, {
          key: "decode",
          value: function decode(id) {
            var ret = [];

            if (!id || !id.length || typeof id !== 'string') {
              return ret;
            }

            return this._decode(id, this.alphabet);
          }
        }, {
          key: "encodeHex",
          value: function encodeHex(hex) {
            hex = hex.toString();

            if (!/^[0-9a-fA-F]+$/.test(hex)) {
              return '';
            }

            var numbers = hex.match(/[\w\W]{1,12}/g);

            for (var i = 0; i !== numbers.length; i++) {
              numbers[i] = parseInt('1' + numbers[i], 16);
            }

            return this.encode.apply(this, numbers);
          }
        }, {
          key: "decodeHex",
          value: function decodeHex(id) {
            var ret = [];
            var numbers = this.decode(id);

            for (var i = 0; i !== numbers.length; i++) {
              ret += numbers[i].toString(16).substr(1);
            }

            return ret;
          }
        }, {
          key: "_encode",
          value: function _encode(numbers) {
            var ret,
                alphabet = this.alphabet,
                numbersIdInt = 0;

            for (var i = 0; i !== numbers.length; i++) {
              numbersIdInt += numbers[i] % (i + 100);
            }

            ret = alphabet.charAt(numbersIdInt % alphabet.length);
            var lottery = ret;

            for (var _i2 = 0; _i2 !== numbers.length; _i2++) {
              var number = numbers[_i2];
              var buffer = lottery + this.salt + alphabet;
              alphabet = this._shuffle(alphabet, buffer.substr(0, alphabet.length));

              var last = this._toAlphabet(number, alphabet);

              ret += last;

              if (_i2 + 1 < numbers.length) {
                number %= last.charCodeAt(0) + _i2;
                var sepsIndex = number % this.seps.length;
                ret += this.seps.charAt(sepsIndex);
              }
            }

            if (ret.length < this.minLength) {
              var guardIndex = (numbersIdInt + ret[0].charCodeAt(0)) % this.guards.length;
              var guard = this.guards[guardIndex];
              ret = guard + ret;

              if (ret.length < this.minLength) {
                guardIndex = (numbersIdInt + ret[2].charCodeAt(0)) % this.guards.length;
                guard = this.guards[guardIndex];
                ret += guard;
              }
            }

            var halfLength = parseInt(alphabet.length / 2, 10);

            while (ret.length < this.minLength) {
              alphabet = this._shuffle(alphabet, alphabet);
              ret = alphabet.substr(halfLength) + ret + alphabet.substr(0, halfLength);
              var excess = ret.length - this.minLength;

              if (excess > 0) {
                ret = ret.substr(excess / 2, this.minLength);
              }
            }

            return ret;
          }
        }, {
          key: "_decode",
          value: function _decode(id, alphabet) {
            var ret = [],
                i = 0,
                r = new RegExp("[".concat(this.escapeRegExp(this.guards), "]"), 'g'),
                idBreakdown = id.replace(r, ' '),
                idArray = idBreakdown.split(' ');

            if (idArray.length === 3 || idArray.length === 2) {
              i = 1;
            }

            idBreakdown = idArray[i];

            if (typeof idBreakdown[0] !== 'undefined') {
              var lottery = idBreakdown[0];
              idBreakdown = idBreakdown.substr(1);
              r = new RegExp("[".concat(this.escapeRegExp(this.seps), "]"), 'g');
              idBreakdown = idBreakdown.replace(r, ' ');
              idArray = idBreakdown.split(' ');

              for (var j = 0; j !== idArray.length; j++) {
                var subId = idArray[j];
                var buffer = lottery + this.salt + alphabet;
                alphabet = this._shuffle(alphabet, buffer.substr(0, alphabet.length));
                ret.push(this._fromAlphabet(subId, alphabet));
              }

              if (this.encode(ret) !== id) {
                ret = [];
              }
            }

            return ret;
          }
        }, {
          key: "_shuffle",
          value: function _shuffle(alphabet, salt) {
            var integer;

            if (!salt.length) {
              return alphabet;
            }

            alphabet = alphabet.split("");

            for (var i = alphabet.length - 1, v = 0, p = 0, j = 0; i > 0; i--, v++) {
              v %= salt.length;
              p += integer = salt.charCodeAt(v);
              j = (integer + v + p) % i;
              var tmp = alphabet[j];
              alphabet[j] = alphabet[i];
              alphabet[i] = tmp;
            }

            alphabet = alphabet.join("");
            return alphabet;
          }
        }, {
          key: "_toAlphabet",
          value: function _toAlphabet(input, alphabet) {
            var id = '';

            do {
              id = alphabet.charAt(input % alphabet.length) + id;
              input = parseInt(input / alphabet.length, 10);
            } while (input);

            return id;
          }
        }, {
          key: "_fromAlphabet",
          value: function _fromAlphabet(input, alphabet) {
            return input.split("").map(function (item) {
              return alphabet.indexOf(item);
            }).reduce(function (carry, item) {
              return carry * alphabet.length + item;
            }, 0);
          }
        }]);

        return Hashids;
      }();

      _exports.default = Hashids;
    });

    // HACK: Lift Hashids onto global object:
    this.Hashids = this.Hashids.default;
    $FUNCTION$ LANGUAGE PLV8 IMMUTABLE STRICT;
  |]
