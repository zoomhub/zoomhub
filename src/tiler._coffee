fs = require 'fs'
path = require 'path'

im = try
    require('gm').subClass {imageMagick: true}
catch err
    null

DZI_PATH = path.join __dirname, '..', 'public', 'dzi'

exports.start = (sourcePath, id, _) ->
    console.log "[tiler] Loading #{sourcePath}"
    overlap = 1
    tileSize = 254
    image = im sourcePath
    size = image.size _
    fullSize =
        width: size.width
        height: size.height
    sizes = []
    sizes.unshift
        width: size.width
        height: size.height
    while size.width > 1 and size.height > 1
        size.width /= 2
        size.height /= 2
        sizes.unshift
            width: Math.max(1, Math.round(size.width))
            height: Math.max(1, Math.round(size.height))
    tilesPath = path.join DZI_PATH, "#{id}_files"
    fs.mkdir tilesPath, _
    for size, i in sizes
        levelPath = path.join tilesPath, "#{i}"
        fs.mkdir levelPath, _
        image.resize size.width, size.height
        outputPath = path.join levelPath, "0_0.jpg"
        try
            image.write outputPath, _
            console.log "[tiler] Successfully wrote #{outputPath}"
        catch err
            console.log "[tiler] Error writing #{outputPath}: #{err}"
            throw err

    dziData = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><Image Format=\"jpg\" "
    dziData += "Overlap=\"#{overlap}\" TileSize=\"#{tileSize}\" "
    dziData += "xmlns=\"http://schemas.microsoft.com/deepzoom/2008\"><Size "
    dziData += "Height=\"#{fullSize.height}\" Width=\"#{fullSize.width}\"/></Image>"
    filePath = path.join DZI_PATH, "#{id}.dzi"
    fs.writeFile filePath, dziData, _
    console.log "[tiler] Successfully completed tiling image"