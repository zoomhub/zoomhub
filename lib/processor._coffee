DeepZoomImage = require 'deepzoomtools'
path = require 'path'


# Defaults
DEFAULT_TILE_SIZE = 254
DEFAULT_TILE_OVERLAP = 1
DEFAULT_FORMAT = 'jpg'


# Public API
module.exports = class Processor
  constructor: (@path) ->
  process: (source, _) ->
    destination = path.join @path, path.basename source
    destination += '.dzi'
    console.log destination
    DeepZoomImage.create _, source, destination, DEFAULT_TILE_SIZE,
      DEFAULT_TILE_OVERLAP, DEFAULT_FORMAT
    destination
