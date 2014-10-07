config = require '../config'
crypto = require 'crypto'
FS = require 'fs'
Path = require 'path'
redis = require 'redis'


## HELPERS

DIR_BY_ID_PATH = Path.join config.DATA_DIR, 'content-by-id'
DIR_BY_URL_PATH = Path.join config.DATA_DIR, 'content-by-url'

DIR_PATH_FROM_URL_TO_ID = Path.relative DIR_BY_URL_PATH, DIR_BY_ID_PATH

# TODO: We should be generating random IDs instead of incrementing them.
NEXT_ID_KEY = 'content:next.id'

URL_HASH_ALGORITHM = 'sha256'
URL_HASH_ENCODING = 'hex'

# TEMP: Simple implementation for just two data stores: Redis and flat files.
USE_REDIS = config.DATA_STORE is 'redis'

# TODO: We should only instantiate this if we're using Redis, but we currently
# do rely on Redis for the "next ID" (see above), even in the flat file case.
# TODO: We should also support auth'ed Redis, which is an async connection.
redisClient = redis.createClient()

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
    Path.join DIR_BY_URL_PATH, "#{hashURL url}.json"

getRedisKeyForId = (id) ->
    "content:id:#{id}"

getRedisKeyForURL = (url) ->
    "content:url:#{hashURL url}"

# Convenience helper to read JSON async'ly (non-blocking, unlike `require`),
# and be robust to the file not being present, returning null in that case.
readJSON = (path, _) ->
    try
        JSON.parse FS.readFile path, 'utf8', _
    catch err
        if err.code is 'ENOENT'
            null
        else
            throw err


## PUBLIC

module.exports = class Content
    constructor: (@id, @url) ->
        @ready = false
        @failed = false
        @progress = 0

        # HACK: These hardcode knowledge of our URLs, embed API, etc.
        @shareUrl = "#{config.BASE_URL}/#{@id}"
        @embedHtml = "<script src='#{@shareUrl}.js?width=auto&height=400px'></script>"
        @dzi =
            url: "#{config.BASE_URL}#{config.STATIC_DIR}#{config.DZI_DIR}/#{@id}.dzi"
            # # TODO: implement these.
            # width: "IMPLEMENT WIDTH"
            # height: "IMPLEMENT HEIGHT"
            # tileSize: "IMPLEMENT TILE SIZE"
            # tileOverlap: "IMPLEMENT TILE OVERLAP"
            # tileFormat: "IMPLEMENT TILE FORMAT"

    # TODO:
    # - markProgress
    # - others?

    markReady: (_) ->
        @ready = true
        @failed = false
        @progress = 1
        @save _

    markFailed: (_) ->
        @ready = false
        @failed = true
        @save _

    save: (_) ->
        # TODO: Can we have saves be precise and isolated?
        # E.g. a change to `progress` would *only* save `progress`,
        # being robust to any concurrent change to other properties.
        if USE_REDIS
            client.mset (getRedisKeyForId @id), @stringify(), _
        else
            FS.writeFile (getFilePathForId @id), @stringify(), _

    # Convenience shortcut to JSON.stringify:
    stringify: ->
        JSON.stringify @, null, (if USE_REDIS then 0 else 4)
            # TEMP: For debugging, pretty-printing JSON in flat file case.

    @getById: (id, _) ->
        if USE_REDIS
            if json = redisClient.get (getRedisKeyForId id), _
                JSON.parse json
            else
                null
        else
            readJSON (getFilePathForId id), _   # returns null if no file

    @getByURL: (url, _) ->
        if USE_REDIS
            if id = redisClient.get (getRedisKeyForURL url), _
                @getById id, _
            else
                null
        else
            # note that our URL files are symlinks to the ID files, and
            # node's FS.readFile() follows symlinks natively. sweet!
            readJSON (getFilePathForURL url), _

    @createFromURL: (url, _) ->
        # TODO: Change this to an optimistic random ID (w/ retry on collision).
        # We need to make sure our writes support proper rollback too though.
        id = (redisClient.incr NEXT_ID_KEY, _).toString()
        content = new Content id, url

        if USE_REDIS
            idKey = getRedisKeyForId id
            urlKey = getRedisKeyForURL url
            client.mset idKey, content.stringify(), urlKey, id, _
        else
            idPath = getFilePathForId id
            urlPath = getFilePathForURL url
            urlToIdPath =
                Path.join DIR_PATH_FROM_URL_TO_ID, Path.basename idPath
            FS.writeFile idPath, content.stringify(), _
            FS.symlink urlToIdPath, urlPath, _

        content
