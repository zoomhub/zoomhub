config = require '../config'
crypto = require 'crypto'
FS = require 'fs'
Path = require 'path'
redis = require 'redis'
URL = require 'url'


## HELPERS

DIR_BY_ID_PATH = Path.join config.DATA_DIR, 'content-by-id'
DIR_BY_URL_PATH = Path.join config.DATA_DIR, 'content-by-url'

# TODO: this is a short term fix to support the content by url code-path
# we should remove this when we move to a proper database
try
    DATABASE = require Path.join DIR_BY_URL_PATH, 'data.json'
catch e
    DATABASE = {}

DIR_PATH_FROM_URL_TO_ID = Path.relative DIR_BY_URL_PATH, DIR_BY_ID_PATH

# TODO: We should be generating random IDs instead of incrementing them.
NEXT_ID_KEY = 'content:next.id'

URL_HASH_ALGORITHM = 'sha256'
URL_HASH_ENCODING = 'hex'

# TEMP: Simple implementation for just two data stores: Redis and flat files.
USE_REDIS = config.DATA_STORE is 'redis'

# TODO: We should also support auth'ed Redis, which is an async connection.
redisClient = redis.createClient() if USE_REDIS

# We hash URLs, to avoid character or length errors.
hashURL = (url) ->
    hash = crypto.createHash URL_HASH_ALGORITHM
    hash.update url
    hash.digest URL_HASH_ENCODING

getFilePathForId = (id) ->
    # NOTE: Since file systems are typically case-insensitive (even though
    # they're case-aware), prefix capital letters with an underscore.
    id = id.replace /([A-Z])/g, '_$1'
    Path.join DIR_BY_ID_PATH, "#{id}.json"

getFilePathForURL = (url) ->
    if idFile = DATABASE["#{hashURL url}.json"]
        Path.join DIR_BY_ID_PATH, idFile.replace '../content-by-id', ''
    else
        ''

getRedisKeyForId = (id) ->
    "content:id:#{id}"

getRedisKeyForURL = (url) ->
    "content:url:#{hashURL url}"

# Convenience helper to read a file async'ly (non-blocking, unlike `require`),
# but return null (rather than throw) if the file doesn't exist.
readFile = (path, _) ->
    try
        FS.readFile path, 'utf8', _
    catch err
        if err.code is 'ENOENT'
            null
        else
            throw err


## PUBLIC

#
# This class represents a piece of ZoomHub content.
# A piece of ZoomHub content associates a source URL with a unique ID,
# along with info about the input data, conversion progress, and output.
#
# This info is what we store in our database, but it's also what we return to
# clients of our API. Importantly, these two use cases are different --
# we don't want to return all info to clients, and conversely, we return extra
# data to clients that's easily derivable and shouldn't be stored.
#
# To achieve both these use cases, this class stores the database data as a
# private object, and exposes property getters on top of it for API clients.
# Importantly, the database data is non-enumerable, so it doesn't get included
# in JSON.stringify.
#
module.exports = class Content

    #
    # Private constructor. Takes database data.
    #
    constructor: (_data={}) ->
        Object.defineProperty @, '_data',
            value: _data
            enumerable: false

        # TODO: Can we have some code that declares/specifies what this
        # database looks like? For now, see public property getters below.
        # In addition to those, we also have `mime` and `size` internally.
        # The `dzi` property is a nested object containing `width`, `height`,
        # `tileSize`, `tileOverlap`, and `tileFormat`.

    ## Properties:

    #
    # Helper to define a group of property getters.
    #
    get = (props) =>
        for prop, getter of props
            Object.defineProperty @::, prop,
                get: getter
                enumerable: true
                configurable: true

    # Public properties (e.g. to API clients):
    get id: -> @_data.id
    get url: -> @_data.url
    get ready: -> @_data.ready ? false
    get failed: -> @_data.failed ? false
    get progress: -> @_data.progress ? 0

    # HACK: These hardcode knowledge of our URLs, embed API, etc.
    get shareUrl: -> "#{config.BASE_URL}/#{@id}"
    get embedHtml: -> "<script src='#{@shareUrl}.js?width=auto&height=400px'></script>"

    # HACK: We clone the `dzi` object to add an `url` property to it.
    # This also hardcodes knowledge of our URLs.
    # TODO: We should save our DZIs with their own custom ID (and store that
    # here in our database), to support cache purge/refresh/etc.
    get dzi: ->
        # Our database files have the DZI info every for image, but we haven't
        # necessarily copied every image's DZI *files*, so we shouldn't return
        # the `dzi` property until we do. Fortunately, we track whether we've
        # copied the files or not with the `ready` property.
        return if not @ready

        raw = @_data.dzi
        return raw if not raw
        clone = {}
        clone.url = URL.resolve config.CONTENT_CDN_URL,
            "#{config.CONTENT_DZIS_PREFIX}#{@id}.dzi"
        clone[prop] = val for prop, val of raw
        clone

    ## Instance methods:

    toJSON: ->
        # The native JSON.stringify doesn't pick up property getters on our
        # *prototype*, which is where they're defined.
        # So help it out by telling it all of our public/enumerable props.
        obj = {}
        obj[prop] = val for prop, val of @
        obj

    # TODO:
    # - markProgress
    # - others?

    markReady: (dzi, _) ->
        @_data.ready = true
        @_data.failed = false
        @_data.progress = 1
        @_data.dzi = dzi
        @_save _

    markFailed: (_) ->
        @_data.ready = false
        @_data.failed = true
        # let `progress` remain where it was
        delete @_data.dzi
        @_save _

    _save: (_) ->
        # TODO: Can we have saves be precise and isolated?
        # E.g. a change to `progress` would *only* save `progress`,
        # being robust to any concurrent change to other properties.
        if USE_REDIS
            client.mset (getRedisKeyForId @id), @_stringifyData(), _
        else
            FS.writeFile (getFilePathForId @id), @_stringifyData(), _

    _stringifyData: ->
        JSON.stringify @_data, null, (if USE_REDIS then 0 else 4)
            # TEMP: For debugging, pretty-printing JSON in flat file case.

    @getById: (id, _) ->
        json =
            if USE_REDIS
                redisClient.get (getRedisKeyForId id), _
            else
                readFile (getFilePathForId id), _

        if json
            new Content JSON.parse json
        else
            null

    @getByURL: (url, _) ->
        if USE_REDIS
            if id = redisClient.get (getRedisKeyForURL url), _
                @getById id, _
            else
                null
        else
            # note that our URL files are symlinks to the ID files, and
            # node's FS.readFile() follows symlinks natively. sweet!
            if json = readFile (getFilePathForURL url), _
                new Content JSON.parse json
            else
                null

    @createFromURL: (url, _) ->
        # TODO: Change this to an optimistic random ID (w/ retry on collision).
        # We need to make sure our writes support proper rollback too though.
        if not redisClient
          throw new Error 'Content.createFromURL() not implemented yet w/out Redis.'

        id = (redisClient.incr NEXT_ID_KEY, _).toString()
        content = new Content {id, url}

        if USE_REDIS
            idKey = getRedisKeyForId id
            urlKey = getRedisKeyForURL url

            client.mset idKey, content._stringifyData(), urlKey, id, _

        else
            idPath = getFilePathForId id
            urlPath = getFilePathForURL url
            urlToIdPath =
                Path.join DIR_PATH_FROM_URL_TO_ID, Path.basename idPath

            FS.writeFile idPath, content._stringifyData(), _
            FS.symlink urlToIdPath, urlPath, _

        content
