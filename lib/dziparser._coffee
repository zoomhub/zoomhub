fs = require 'fs'
xml2js = require 'xml2js'
processor = require './processor'

#Public API
module.exports = class DZIParser
  @parse = (path, _) ->
    try
      dzi = xml2js.parseString fs.readFile(path, _), _
      size = dzi.Image.Size[0].$ # xml2js is gross sometimes
      result =
        tileSize: dzi.Image.$.TileSize
        tileOverlap: dzi.Image.$.Overlap
        tileFormat: dzi.Image.$.Format
        width: size.Width
        height: size.Height
    catch error
      if error.code == ENOENT # The DZI hasnt' been created yet, so use defaults
        result =
          tileSize: processor.DEFAULT_TILE_SIZE
          tileOverlap: processor.DEFAULT_TILE_OVERLAP
          tileFormat: processor.DEFAULT_FORMAT
    finally
      return result

