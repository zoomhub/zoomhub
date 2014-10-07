#!/usr/bin/env _coffee

ez = require 'ez-streams'
fs = require 'fs'
Path = require 'path'
pkgcloud = require 'pkgcloud'
Request = require 'request'
URL = require 'url'


# Constants
CONTAINER = 'content'
CONTENT_TYPE =
  '.dzi': 'application/xml'
  '.png': 'image/png'
  '.jpg': 'image/jpeg'

# Initialization
FILE = process.argv[2]
USERNAME = process.env.RACKSPACE_API_USER
API_KEY = process.env.RACKSPACE_API_KEY

if not USERNAME
  console.error 'Please provide `RACKSPACE_API_USER` environment variable.'
  process.exit 1

if not API_KEY
  console.error 'Please provide `RACKSPACE_API_KEY` environment variable.'
  process.exit 1

client = pkgcloud.storage.createClient
    provider: 'rackspace'
    username: USERNAME
    apiKey: API_KEY
    region: 'ORD' # Chicago

sources = (fs.readFileSync FILE).toString().split '\n'

for source in sources when source.length > 0
  try
    destination = URL.parse(source).path.replace /// ^/content ///, '/images-test2'
    extension = Path.extname source
    contentType = CONTENT_TYPE[extension]

    if not contentType
      console.error "Unknown file extension: #{extension}"
      process.exit 1

    console.log "Migrating #{source}"
    [response] = Request.get source, [_]
    contentLength = response.headers['content-length']
    options =
      container: CONTAINER
      remote: destination
      headers:
        'content-type': contentType
        'content-length': contentLength

    reader = ez.devices.node.reader Request.get source
    writer = ez.devices.node.writer client.upload options
    reader.pipe _, writer
  catch error
    console.error "Error: #{source}", error.stack or error
