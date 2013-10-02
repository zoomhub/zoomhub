path = require 'path'

im = try
    require('gm').subClass {imageMagick: true}
catch err
    null

DZI_PATH = path.join __dirname, '..', 'public', 'dzi'

exports.start = (sourcePath, _) ->
    console.log "[tiler] Loading #{sourcePath}"
    image = im sourcePath
    size = image.size _
    console.log size
    image.resize 50, 50
    outputPath = path.join DZI_PATH, 'tiler-test.jpg'
    console.log "[tiler] Writing to #{outputPath}"
    try
        image.write outputPath, _
        console.log "[tiler] Successfully wrote #{outputPath}"
    catch err
        console.log "[tiler] Error writing #{outputPath}: #{err}"
        throw err
