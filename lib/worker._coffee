#
# This is the main (and only, for now) ZoomHub worker. It processes all content,
# from start to finish.
#
# Over time, we may split this out into separate files, particularly to handle
# different types of content.
#
# This should eventually run on its own and poll a queue, but for now,
# since we have no queue, it should be called to process content manually.
#
# Usage:
#
#   worker = require '/path/to/this/file'
#   worker.process content, _
#
# TODO: We'll want to tweak this contract when we add a proper queue, since
# e.g. we'll want `process` to delete `content` from the queue on success,
# which means it'll probably need to take a queue receipt too.
#

config = require '../config'
crypto = require 'crypto'
DeepZoomImage = require 'deepzoomtools'
DZIParser = require './dziparser'
FS = require 'fs-extra'
OS = require 'os'
Path = require 'path'
Request = require 'request'
storageClient = require './util/storageClient'


BASE_TEMP_DIR = Path.join OS.tmpdir(), 'zoomhub-worker'

# TODO: Tune this. And should this be a config? Or unnecessary?
# NOTE: Levels are also parallelized, unlimited. Documentation in `uploadDZI`.
PARALLEL_UPLOADS_PER_LEVEL = 4

XML_CONTENT_TYPE = 'application/xml'

## PUBLIC:

#
# Fully process the given content (an instance of the `Content` class).
#
# NOTE: This method aims to *not* throw (or call the callback with) any errors.
# It aims to handle all errors gracefully (e.g. logging them to stderr), and
# take care of marking the content as `failed`.
#
# When this finishes, the content will always be either `ready` or `failed`.
#
# TODO: We want to also update the content's `progress` here as we process it.
#
@process = (content, _) ->
    try
        {id, url} = content
        echo 'Processing content... id=%j, url=%j', id, url

        if content.ready
            # TODO: Assert that other properties have expected values?
            # E.g. `failed: false`, `progress: 1`, `typeof dzi is 'object'`, ...
            echo 'Content already successfully processed. id=%j', id
            return

        if content._isActive
            echo 'Content already being worked on currently. id=%j', id
            return

        echo 'Marking content as active... id=%j', id
        content._markActive _

        echo 'Creating temp dir... id=%j', id
        dir = makeTempDir _

        # Download the content to a file (within our temp dir) named by the ID.
        #
        # NOTE: We explicitly add an arbitrary extension, due to this VIPS bug:
        # https://github.com/jcupitt/libvips/issues/242
        #
        file = Path.join dir, content.id + '.raw'
        echo 'Downloading content to temp dir... id=%j, file=%j', id, file
        downloadFile content.url, file, _

        echo 'Generating DZI for content... id=%j', id
        dziPath = createDZI file, _

        echo 'Parsing the generated DZI... id=%j', id
        dzi = DZIParser.parse dziPath, _

        echo 'Uploading the generated DZI...
            id=%j, width=%j, height=%j, format=%j',
            id, dzi.width, dzi.height, dzi.tileFormat
        uploadDZI dziPath, _

        echo 'Success! Marking content as ready... id=%j', id
        content.markReady dzi, _

    catch err
        erro 'Error processing content! Marking content as failed...
            id=%j, error=%s', id, err.stack or err
        content.markFailed _

    finally
        # Clean up temp files we created, if any:
        # (It'd be nice to have them for debugging, but we can't risk eating up
        # disk space on our server, given that this is a long-running process.)
        if dir
            echo 'Cleaning up... id=%j', id
            FS.remove dir, _ if dir

        echo 'Finished processing content. id=%j', id


## HELPERS:

# TODO: It'd be better to use a proper and standard logger eventually instead,
# but that'll also be more useful when we actually have log aggregation.
log = (level='info', msg='', args...) ->
    timestamp = new Date().toISOString()
    msg = "#{timestamp} [Worker] #{msg}"
    console[level] msg, args...

echo = log.bind @, 'info'
erro = log.bind @, 'error'

makeTempDir = (_) ->
    rand = crypto.randomBytes(16).toString('hex')
    path = Path.join BASE_TEMP_DIR, rand
    FS.mkdirp path, _
    path

# TODO: Find some way to report progress here, across read + write.
downloadFile = (url, file, cb) ->
    req = Request.get url
    req.on 'error', cb
    req.on 'response', (resp) ->
        if resp.statusCode >= 400
            req.abort()
            # NOTE: At this point, the body hasn't been received yet!
            # TODO: Would it be better to wait til the body has been received,
            # so we can include it in the error message?
            cb new Error "#{resp.statusCode} response from #{url}"

     file = FS.createWriteStream file
     file.on 'error', cb
     file.on 'finish', cb

     req.pipe file

# Automatically derives and returns the name of the destination DZI.
# TODO: Find some way to report progress here.
createDZI = (src, _) ->
    dest = getSiblingFile src, '.dzi'
    DeepZoomImage.create _, src, dest
    dest

# TODO: Find some way to report progress here.
uploadDZI = (dzi, _) ->
    # All files will be uploaded to storage with a name relative to the base
    # directory containing this DZI:
    baseDir = Path.dirname dzi
    baseName = Path.basename dzi

    # Read all the levels in the tiles directory:
    tilesDir = getSiblingFile dzi, '_files'
    levels = FS.readdir tilesDir, _

    # For each level in parallel...
    #
    # NOTE: Not concerned about too much parallelism here.
    # The number of levels is O(log n) relative to the size of an image, and
    # most of the lower levels have only one tile, so they'll finish quickly.
    # Instead, we'll limit the parallelism per level below, for higher levels.
    #
    # NOTE: This syntax is Streamline convenience for parallelism:
    # https://github.com/Sage/streamlinejs/blob/master/lib/compiler/builtins.md
    #
    levels.forEach_ _, Infinity, (_, level) ->

        # Read the tiles in this level directory:
        levelDir = Path.join tilesDir, level
        tiles = FS.readdir levelDir, _

        # Upload each tile in parallel, within a reasonable limit...
        tiles.forEach_ _, PARALLEL_UPLOADS_PER_LEVEL, (_, tile) ->
            tilePath = Path.join levelDir, tile
            tileName = Path.relative baseDir, tilePath

            uploadDZIFile tilePath, tileName, _

    # Finally, upload the main .dzi file:
    # TODO: This could be parallelized with the above, but not significant.
    uploadDZIFile dzi, baseName, XML_CONTENT_TYPE, _

uploadDZIFile = (src, dest, type, cb) ->
    # Type is optional; if omitted, defaults to auto-detect:
    if arguments.length is 3
        [src, dest, cb, type] = arguments

    upload = storageClient.upload
        container: config.CONTENT_CONTAINER_NAME
        remote: "#{config.CONTENT_DZIS_PREFIX}#{dest}"
        contentType: type   # If not given, defaults to auto-detect

    upload.on 'error', cb
    upload.on 'success', (file) -> cb null

    file = FS.createReadStream src
    file.on 'error', cb

    file.pipe upload

# Given a path to a file, gets a "sibling" file with the same basename but the
# given different extension. (The extension can also be an arbitrary suffix.)
getSiblingFile = (path, extNew) ->
    dir = Path.dirname path
    extOld = Path.extname path
    base = Path.basename path, extOld
    Path.join dir, "#{base}#{extNew}"


## MAIN:

if module is require.main
    console.error 'This worker cannot run standalone yet, as there is no queue!
        See the documentation at the top of this file for details.'
    process.exit 1
