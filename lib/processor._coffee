DeepZoomImage = require 'deepzoomtools'
path = require 'path'

# Public API
module.exports = class Processor
# Defaults
    @DEFAULT_TILE_SIZE = 254
    @DEFAULT_TILE_OVERLAP = 1
    @DEFAULT_FORMAT = 'jpg'

  constructor: (@path) ->
  process: (source, _) ->
    destination = path.join @path, path.basename source
    destination += '.dzi'
    DeepZoomImage.create _, source, destination, DEFAULT_TILE_SIZE,
      DEFAULT_TILE_OVERLAP, DEFAULT_FORMAT
    destination
