path = require 'path'

im = try
    require('gm').subClass {imageMagick: true}
catch err
    null

STATIC_PATH = path.join __dirname, '..', 'public'

exports.start = (sourcePath, callback) ->
    console.log "[tiler] Loading #{sourcePath}"
    image = im sourcePath
    image.resize 50, 50
    outputPath = path.join STATIC_PATH, 'tiler-test.jpg'
    console.log "[tiler] Writing to #{outputPath}"
    image.write outputPath, (err) ->
        if err
            console.log "[tiler] Error writing #{outputPath}: #{err}"
        else
            console.log "[tiler] Successfully wrote #{outputPath}"
        callback err
