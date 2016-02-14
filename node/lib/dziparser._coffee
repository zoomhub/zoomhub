fs = require 'fs'
xml2js = require 'xml2js'

toInt = (str) ->
    parseInt str, 10

#Public API
module.exports = class DZIParser
    @parse = (path, _) ->
        dzi = xml2js.parseString fs.readFile(path, _), _

        # xml2js is gross sometimes:
        $image = dzi.Image.$
        $size = dzi.Image.Size[0].$

        tileSize: toInt $image.TileSize
        tileOverlap: toInt $image.Overlap
        tileFormat: $image.Format
        width: toInt $size.Width
        height: toInt $size.Height
