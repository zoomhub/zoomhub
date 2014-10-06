#!/usr/bin/env _coffee
#
# To the existing content-by-id data:
#
# - Adds an `id` property.
#
# - Adds content-by-url symlinks.
#

config = require '../config'
crypto = require 'crypto'
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

# Now go through each existing content by ID, and process it:
for idFileName in FS.readdir DIR_BY_ID_PATH, _
    # Skip non-content files:
    continue if (Path.extname idFileName) isnt '.json'

    id = getIdForFilePath idFileName
    idPath = getFilePathForId id  # full path, not just file name

    # Add an `id` property to the data, by reading, modifying, then writing it.
    # Use a new object to ensure that the `id` property is the first property.
    # Also add a stub for the `url` property to ensure that it's second.
    data = {id, url: null}
    for prop, val of require idPath     # require() auto-parses JSON!
        data[prop] = val
    FS.writeFile idPath, (JSON.stringify data, null, 4), _

    # Generate a content-by-url symlink.
    urlPath = getFilePathForURL data.url
    urlToIdPath = Path.join DIR_PATH_FROM_URL_TO_ID, Path.basename idPath
    try
        FS.symlink urlToIdPath, urlPath, _
    catch err
        throw err unless err.code is 'EEXIST'
