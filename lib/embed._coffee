fs = require 'fs'
path = require 'path'
config = require '../config'
dziparser = require './dziparser'

seadragon_path = '/js/openseadragon.min.js'

inject_viewer = "var el = document.createElement('div');
                el.setAttribute('id', '__seadragon1');
                document.body.appendChild(el);

                var viewer = OpenSeadragon({
                    id: '__seadragon1',
                    prefixUrl: '/static/js/images/',
                    tileSources: tileSource
                });"

createTileSourceBlock = (basePath, dzi, _) ->
    pathFragment = path.join config.DZI_DIR, "#{dzi}"
    attribs = dziparser.parse path.join(basePath, "#{pathFragment}.dzi"), _

    if not attribs.ready
        pathFragment = '/queued'
        attribs = dziparser.parse path.join(basePath, "#{pathFragment}.dzi"), _

    "var tileSource = {
        Image: {
            xmlns: 'http://schemas.microsoft.com/deepzoom/2008',
            Url: '#{config.STATIC_DIR}#{pathFragment}_files/',
            Format: '#{attribs.tileFormat}',
            Overlap: '#{attribs.tileOverlap}',
            TileSize: '#{attribs.tileSize}',
            Size: {
                Height: '#{attribs.height}',
                Width: '#{attribs.width}'
            }
        }
    };"

# Public API
module.exports = class Embed
  constructor: (@path) ->

  generate: (@id, _) ->
    result = fs.readFile (path.join @path, seadragon_path), _
    result += createTileSourceBlock @path, @id, _
    result += inject_viewer
