fs = require 'fs'
xml2js = require 'xml2js'

#Public API
module.exports = class DZIParser
    @parse = (path, _) ->
        dzi = xml2js.parseString fs.readFile(path, _), _

        # xml2js is gross sometimes:
        $image = dzi.Image.$
        $size = dzi.Image.Size[0].$

        tileSize: $image.TileSize
        tileOverlap: $image.Overlap
        tileFormat: $image.Format
        width: $size.Width
        height: $size.Height
