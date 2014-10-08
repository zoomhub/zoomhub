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
crypto = require 'crypto'
echo = console.log
flows = require 'streamline/lib/util/flows'
FS = require 'fs'
Path = require 'path'

DIR_BY_ID_PATH = Path.join config.DATA_DIR, 'content-by-id'
DIR_BY_URL_PATH = Path.join config.DATA_DIR, 'content-by-url'

DIR_PATH_FROM_URL_TO_ID = Path.relative DIR_BY_URL_PATH, DIR_BY_ID_PATH

# How many files to process in parallel:
NUM_PARALLEL = 100

# How frequently to log progress:
LOG_FREQUENCY_SECS = 60

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

# Now query all the IDs we have.
# NOTE: This takes up a metric shit ton of memory if lots of files.
# To compensate, we remove filenames from this array as we process them.
echo 'Querying content files...'
idFileNames = FS.readdir DIR_BY_ID_PATH, _
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
    idPath = getFilePathForId id  # full path, not just file name

    # Massage the data by reading it, modifying it, then writing it back.
    # NOTE: This assumes full knowledge of the starting data.
    data = require idPath   # require() auto-parses JSON!
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
        remaining = idFileNames.length
        percent = 100 * (totalNumIds - remaining) / totalNumIds
        echo "#{percent.toFixed 2}% processed; #{remaining} files remaining."
        nextLogTime = Date.now() + 1000 * LOG_FREQUENCY_SECS

echo 'All done!'
