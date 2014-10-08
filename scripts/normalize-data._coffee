#!/usr/bin/env _coffee
#
# To the existing content-by-id data:
#
# - Adds an `id` property, matching the filename (minus uppercase transform).
#
# - Adds our `ready`, `failed`, and `progress` properties.
#   Sets all content to a "queued" state (`ready: false`, `progress: 0`);
#   we can mark as finished once we've copied imagery over too.
#   FIXME: This is *not* safely idempotent once we copy images over!
#   But there isn't any better way to do this, as we already have ready: true.
#
# - Moves the image info into a `dzi` container, if it exists.
#   Update: we had a bug here, so also fixes that bug.
#
# - Fixes the `tileSize` and `tileOverlap` properties from strings to ints.
#
# - Adds content-by-url symlinks.
#

config = require '../config'
CP = require 'child_process'
crypto = require 'crypto'
echo = console.log
FS = require 'fs'
Path = require 'path'

DIR_BY_ID_PATH = Path.join config.DATA_DIR, 'content-by-id'
DIR_BY_URL_PATH = Path.join config.DATA_DIR, 'content-by-url'

DIR_PATH_FROM_URL_TO_ID = Path.relative DIR_BY_URL_PATH, DIR_BY_ID_PATH

getFilePathForId = (id) ->
    # NOTE: Since file systems are typically case-insensitive (even though
    # they're case-aware), prefix capital letters with an underscore.
    id = id.replace /([A-Z])/g, '_$1'
    Path.join DIR_BY_ID_PATH, "#{id}.json"

getFilePathForURL = (url) ->
    # Hash the URL, to avoid character or length errors.
    hash = crypto.createHash 'sha256'
    hash.update url
    hash = hash.digest 'hex'
    Path.join DIR_BY_URL_PATH, "#{hash}.json"

getIdForFilePath = (path) ->
    # Reverses the transform done in `getFilePathForId` above.
    path = path.replace /_([A-Z])/g, '$1'
    Path.basename path, Path.extname path

# For any async errors we encounter -- fail fast:
handleError = (err) ->
    throw err if err

#
# IMPORTANT: Listing all the files into memory via FS.readdir consumes a *ton*
# of memory, so we use Bash to do that, and process the result as a stream.
#
# Calls the given function for each file, and the final callback when done.
# The function is called as (fileName, i, _).
# Note that any unhandled errors from the function will fail fast and crash.
#
# FIXME: Our processing of the data likely won't keep up with the speed of
# ls output, which means we might get a very large fan-out of processing...
# which means we'd still be taking up a ton of memory. Can we synchronize?
#
listFiles = (path, each, done) ->
    ls = CP.spawn 'ls', ['-f', path]

    i = 0   # keep a running count, like array index
    ls.stdout.setEncoding 'utf8'
    ls.stdout.on 'data', (stdout) ->
        return if not stdout
        for file in stdout.split '\n'
            each file, i, handleError
            i++

    err = ''    # aggregate stderr output in case of error
    ls.stderr.setEncoding 'utf8'
    ls.stderr.on 'data', (stderr) ->
        err += stderr

    ls.on 'close', (code) ->
        if code
            done new Error "Error listing files (exit code #{code}): #{err}"
        else
            done null

# Now list all the IDs once to count the total...
echo 'Querying content files...'
totalNumIds = 0
listFiles DIR_BY_ID_PATH, (-> totalNumIds++), _
echo "#{totalNumIds} files found."

# Then list them again to actually process them, outputting progress every min:
echo 'Processing...'
nextLogTime = Date.now() + 1000 * 60
listFiles DIR_BY_ID_PATH, (idFileName, i, _) ->
    # Skip non-content files:
    return if (Path.extname idFileName) isnt '.json'

    id = getIdForFilePath idFileName
    idPath = getFilePathForId id  # full path, not just file name

    # Massage the data by reading it, modifying it, then writing it back.
    # NOTE: This assumes full knowledge of the starting data.
    data = JSON.parse FS.readFile idPath, 'utf8', _
    failed = not (data.width and data.height and data.tileSize) and
        # if we're re-running this after our conversion:
        not (data.dzi?.width and data.dzi?.height and data.dzi?.tileSize)
    data =
        id: id
        url: data.url
        ready: false        # FIXME: Remove these hardcodings after we've
        failed: failed      # corrected production data with this script.
        progress: 0         # We should then respect the input data.
        mime: data.mime
        size: data.size
        dzi: data.dzi or {  # braces needed otherwise CS syntax error
            width: data.width
            height: data.height
            tileSize: parseInt data.tileSize, 10
            tileOverlap: parseInt data.tileOverlap, 10
            tileFormat: data.tileFormat
        } if not failed
    delete data.dzi if failed   # fix bug we made
    FS.writeFile idPath, (JSON.stringify data, null, 4), _

    # Generate a content-by-url symlink.
    urlPath = getFilePathForURL data.url
    urlToIdPath = Path.join DIR_PATH_FROM_URL_TO_ID, Path.basename idPath
    try
        FS.symlink urlToIdPath, urlPath, _
    catch err
        throw err unless err.code is 'EEXIST'

    if Date.now() >= nextLogTime
        percent = 100 * (i + 1) / totalNumIds
        remaining = totalNumIds - (i + 1)
        echo "#{percent.toFixed 2}% processed; #{remaining} files remaining."
        nextLogTime = Date.now() + 1000 * 60
, _

process.on 'exit', ->
    echo 'All done!'
