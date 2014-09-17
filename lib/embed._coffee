fs = require 'fs'
path = require 'path'

seadragon_path = '/js/openseadragon.min.js'

inject_viewer = "var el = document.createElement('div');
                el.setAttribute('id', '__seadragon1');
                document.body.appendChild(el);

                var viewer = OpenSeadragon({
                    id: '__seadragon1',
                    prefixUrl: '/static/js/images/',
                    tileSources: tileSource
                });"

createTileSourceBlock = (dzi) ->
    "var tileSource = {
        Image: {
            xmlns: 'http://schemas.microsoft.com/deepzoom/2008',
            Url: '/static/dzi/#{dzi}.dzi',
            Format: 'TODO',
            Overlap: 'TODO',
            TileSize: 'TODO',
            Size: {
                Height: 'TODO',
                Width: 'TODO'
            }
        }
    };"

# Public API
module.exports = class Embed
  constructor: (@path) ->

  generate: (@id, _) ->
    result = fs.readFile (path.join @path, seadragon_path), _
    result += createTileSourceBlock @id
    result += inject_viewer
