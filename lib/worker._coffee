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

BASE_TEMP_DIR = Path.join OS.tmpdir(), 'zoomhub-worker'


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
        dzi = createDZI file, _

        echo 'Parsing the generated DZI... id=%j', id
        dzi = DZIParser.parse dzi, _

        # TODO: Upload!

        # erro 'Success! Marking content as ready... id=%j', id
        # content.markReady dzi, _

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
    dir = Path.dirname src
    ext = Path.extname src
    base = Path.basename src, ext
    dest = Path.join dir, "#{base}.dzi"

    DeepZoomImage.create _, src, dest

    dest


## MAIN:

if module is require.main
    console.error 'This worker cannot run standalone yet, as there is no queue!
        See the documentation at the top of this file for details.'
    process.exit 1
