fs = require 'fs'
Path = require 'path'
config = require '../config'
dziparser = require './dziparser'


## CONSTANTS

SEADRAGON_JS_PATH = Path.join config.STATIC_PATH, 'js', 'openseadragon.min.js'

QUEUED_DZI_XML_PATH = Path.join config.STATIC_PATH, 'queued.dzi'
QUEUED_DZI_XML_URL = config.BASE_URL + config.STATIC_DIR + '/queued.dzi'

VIEWER_JS = """
    var el = document.createElement('div');
    el.setAttribute('id', '__seadragon1');
    document.body.appendChild(el);

    var viewer = OpenSeadragon({
        id: '__seadragon1',
        prefixUrl: '/static/js/images/',
        tileSources: tileSource
    });
"""


## HELPERS

getTilesURL = (xmlURL) ->
    xmlURL.replace '.dzi', '_files/'

createTileSourceBlock = (basePath, dzi, _) ->
    # HACK: If the DZI isn't ready yet, use our stand-in "queued" DZI:
    if not dzi?.url
        dzi = dziparser.parse QUEUED_DZI_XML_PATH, _
        dzi.url = QUEUED_DZI_XML_URL

    tileSource =
        Image:
            xmlns: 'http://schemas.microsoft.com/deepzoom/2008'
            Url: getTilesURL dzi.url
            Format: dzi.tileFormat
            Overlap: dzi.tileOverlap
            TileSize: dzi.tileSize
            Size:
                Width: dzi.width
                Height: dzi.height

    return "var tileSource = #{JSON.stringify tileSource, null, 4};"


## PUBLIC

@generate = (content, _) ->
    result = [
        fs.readFile SEADRAGON_JS_PATH, _
        createTileSourceBlock @path, content.dzi, _
        VIEWER_JS
    ].join ';\n'
