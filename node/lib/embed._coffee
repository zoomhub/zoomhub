fs = require 'fs'
Path = require 'path'
config = require '../config'
dziparser = require './dziparser'
URL = require 'url'


## CONSTANTS

SEADRAGON_JS_FILE_PATH = Path.join config.STATIC_FILE_PATH,
    'lib', 'openseadragon', 'openseadragon.min.js'

QUEUED_DZI_XML_FILE_PATH = Path.join config.STATIC_FILE_PATH, 'queued.dzi'
QUEUED_DZI_XML_URL = config.BASE_URL + config.STATIC_URL_PATH + '/queued.dzi'

VIEWER_IMAGES_URL = URL.resolve config.CONTENT_CDN_URL, 'openseadragon-images/'

CLASS_NAME = '__seadragon'


## HELPERS

getTilesURL = (xmlURL) ->
    xmlURL.replace '.dzi', '_files/'

getRandomId = ->
    "#{Math.random()}"[2..]

createTileSourceBlock = (dzi, opts={}, _) ->
    # HACK: If the DZI isn't ready yet, use our stand-in "queued" DZI:
    if not dzi?.url
        dzi = dziparser.parse QUEUED_DZI_XML_FILE_PATH, _
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

    # Embed snippet:
    #
    # - We use document.write, to support writing into any container, rather
    #   than assuming the user always wants this directly inside <body>.
    #
    # - We don't create any local variables today, but to be future-proof and
    #   robust, we do all our work inside a closure, to avoid pollution.
    #
    # - We generate a random ID to support multiple embeds in the page,
    #   but we also support supplying an ID. TODO: The old Zoom.it embed used
    #   an incremental ID so it was predictable; can we support that too?
    #
    # - We have a default width & height, but support supplying those too.
    #   The rest of the styles are copied from the old Zoom.it embed.
    #   TODO: Do we need those anymore with OpenSeadragon though?
    #
    {id, width, height} = opts
    id or= "#{CLASS_NAME}#{getRandomId()}"
    width or= 'auto'
    height or= '400px'

    style = "
        border: 1px solid black; background: black; color: white;
        width: #{width}; height: #{height}; margin: 0; padding: 0;
    "
    html = "
        <div class='#{CLASS_NAME}' id='#{id}' style='#{style}'></div>
    "
    viewerInfo =
        id: id
        prefixUrl: VIEWER_IMAGES_URL
        tileSources: tileSource     # note the plural/singular discrepancy!

    return """
        (function () {
            document.write(#{JSON.stringify html});
            OpenSeadragon(#{JSON.stringify viewerInfo});
        })();
    """


## PUBLIC

@generate = (content, _, opts={}) ->
    result = [
        fs.readFile SEADRAGON_JS_FILE_PATH, _
        createTileSourceBlock content.dzi, opts, _
    ].join ';\n'
