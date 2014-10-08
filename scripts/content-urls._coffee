#!/usr/bin/env _coffee

DeepZoomImage = require 'deepzoomtools'
Request = require 'request'
xml2js = require 'xml2js'


# Helpers
num = (x) ->
  parseInt x, 10

# Inputs
ID = process.argv[2]

if not ID
  console.error "ID argument required"
  process.exit 1

# Note: Disable `strict` mode due to XML declaration:
parser = new xml2js.Parser {strict: false}

# Main
source = "http://cache.zoom.it/content/#{ID}.dzi"

try
  [..., body] = Request.get source, [_]
  {IMAGE:
      $:
        TILESIZE: tileSize
        OVERLAP: overlap
        FORMAT: format
      SIZE: [
        $:
          WIDTH: width
          HEIGHT: height
      ]
  } = parser.parseString body, _
  width = num width
  height = num height
  tileSize = num tileSize
  tileOverlap = num tileOverlap

  descriptor = new DeepZoomImage source, width, height, tileSize,
    tileOverlap, format

  console.log source
  for level, index in descriptor.levels
    for column in [0...level.numColumns]
      for row in [0...level.numRows]
        tile = descriptor.getTile index, column, row
        console.log tile.url

catch error
  console.error "Error", error.stack or error
