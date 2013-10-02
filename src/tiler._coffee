path = require 'path'

im = try
    require('gm').subClass {imageMagick: true}
catch err
    null

DZI_PATH = path.join __dirname, '..', 'public', 'dzi'

exports.start = (sourcePath, callback) ->
    console.log "[tiler] Loading #{sourcePath}"
    image = im sourcePath
    image.resize 50, 50
    outputPath = path.join DZI_PATH, 'tiler-test.jpg'
    console.log "[tiler] Writing to #{outputPath}"
    image.write outputPath, (err) ->
        if err
            console.log "[tiler] Error writing #{outputPath}: #{err}"
        else
            console.log "[tiler] Successfully wrote #{outputPath}"
        callback err
