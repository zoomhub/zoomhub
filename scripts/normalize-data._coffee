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
# Note: for perf and resumability, deletes files from the input dir as it goes.
# This is the reason for step 1 below, creating a copy.
#
# Usage:
#
# 1. Unzip zoomhub.zip to a folder named `input-by-id`. (Do this even if this
#    data has already been unzipped to a `content-by-id` directory.)
#
# 2. Run this script.
#
# 3. If the script exits prematurely, without outputting "done", re-run.
#

config = require '../config'
crypto = require 'crypto'
echo = console.log
flows = require 'streamline/lib/util/flows'
FS = require 'fs'
Path = require 'path'

INPUT_DIR_BY_ID_PATH = Path.join config.DATA_DIR, 'input-by-id'

OUTPUT_DIR_BY_ID_PATH = Path.join config.DATA_DIR, 'content-by-id'
OUTPUT_DIR_BY_URL_PATH = Path.join config.DATA_DIR, 'content-by-url'

OUTPUT_DIR_REL_PATH_FROM_URL_TO_ID =
    Path.relative OUTPUT_DIR_BY_URL_PATH, OUTPUT_DIR_BY_ID_PATH

# How many files to process in parallel:
NUM_PARALLEL = 100

# How frequently to log progress:
LOG_FREQUENCY_SECS = 60

_getFilePathForId = (id, dir) ->
    # NOTE: Since file systems are typically case-insensitive (even though
    # they're case-aware), prefix capital letters with an underscore.
    id = id.replace /([A-Z])/g, '_$1'
    Path.join dir, "#{id}.json"

getInputFilePathForId = (id) ->
    _getFilePathForId id, INPUT_DIR_BY_ID_PATH

getOutputFilePathForId = (id) ->
    _getFilePathForId id, OUTPUT_DIR_BY_ID_PATH

getOutputFilePathForURL = (url) ->
    # Hash the URL, to avoid character or length errors.
    hash = crypto.createHash 'sha256'
    hash.update url
    hash = hash.digest 'hex'
    Path.join OUTPUT_DIR_BY_URL_PATH, "#{hash}.json"

getIdForFilePath = (path) ->
    # Reverses the transform done in `_getFilePathForId` above.
    path = path.replace /_([A-Z])/g, '$1'
    Path.basename path, Path.extname path

# Now query all the IDs we have.
# NOTE: This takes up a metric shit ton of memory if lots of files.
# To compensate, we remove filenames from this array as we process them.
echo 'Querying content files...'
idFileNames = FS.readdir INPUT_DIR_BY_ID_PATH, _
totalNumIds = idFileNames.length
echo "#{totalNumIds} files found."

# Now go through each ID,and process it, outputting percentage every minute.
# Since we're going to be I/O bound here, we parallelize heavily.
echo 'Processing...'
funnel = flows.funnel NUM_PARALLEL
nextLogTime = Date.now() + 1000 * LOG_FREQUENCY_SECS

while idFileNames.length then funnel _, (_) ->
    # Grab an ID file and remove it from our array (to reduce memory usage).
    # Skip non-content files.
    idFileName = idFileNames.pop()
    return if (Path.extname idFileName) isnt '.json'

    id = getIdForFilePath idFileName
    inputIdPath = getInputFilePathForId id
    outputIdPath = getOutputFilePathForId id

    # Massage the data by reading it, modifying it, then writing it back.
    # NOTE: This assumes full knowledge of the starting data.
    data = JSON.parse FS.readFile inputIdPath, 'utf8', _
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
    FS.writeFile outputIdPath, (JSON.stringify data, null, 4), _
    FS.unlink inputIdPath, _

    # Generate a content-by-url symlink.
    outputUrlPath = getOutputFilePathForURL data.url
    outputUrlToIdPath = Path.join \
        OUTPUT_DIR_REL_PATH_FROM_URL_TO_ID, Path.basename outputIdPath
    try
        FS.symlink outputUrlToIdPath, outputUrlPath, _
    catch err
        throw err unless err.code is 'EEXIST'

    if Date.now() >= nextLogTime
        remaining = idFileNames.length
        percent = 100 * (totalNumIds - remaining) / totalNumIds
        echo "#{percent.toFixed 2}% processed; #{remaining} files remaining.
            (Last processed: #{id} / #{idFileName})"
        nextLogTime = Date.now() + 1000 * LOG_FREQUENCY_SECS

echo 'All done!'
