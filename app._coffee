config = require './config'
Content = require './lib/content'
express = require 'express'
Fetcher = require './lib/fetcher'
fs = require 'fs'
jade = require 'jade'
path = require 'path'
Processor = require './lib/processor'


# Constants
PIPELINE_PATH = path.join __dirname, 'pipeline'
STATIC_PATH = path.join __dirname, 'public'
STATIC_URL = '/static'

# Fetcher
FETCHER_PATH = path.join PIPELINE_PATH, 'fetcher'
fetcher = new Fetcher FETCHER_PATH

# Processor
DZI_PATH = path.join STATIC_PATH, 'dzi'
processor = new Processor DZI_PATH


## APP:

app = require('streamline-express') express()

# Template engine
app.engine 'jade', jade.__express
app.set 'view engine', 'jade'
app.set 'views', path.join __dirname, 'views'


## MIDDLEWARE:
# (see http://www.senchalabs.org/connect/ for reference)

# favicon reqs are very common; respond to those immediately:
app.use express.favicon()   # will use default favicon since we don't specify

# serve static files (and register DZI as an XML type):
express.static.mime.types.dzi = 'application/xml'
app.use '/static', express.static STATIC_PATH

# otherwise, log all requests:
app.use express.logger config.EXPRESS_LOGGER_FORMAT

# parse request bodies, e.g. JSON:
# (we don't need this yet, but it's easy to add and does no harm)
# (note also that this incorporates middleware to limit reqs to 1 MB. nice.)
app.use express.bodyParser()

# gzip response bodies (only kicks in after a size threshold):
# (we don't need this yet either, but it's also easy and harmless)
app.use express.compress()

# TODO we'll want middleware to implement CORS eventually.

# then run our routes, before the error handler in case there are errors:
app.use app.router

# TODO we need error handling middleware, but we want to return well-defined
# error JSON objects since we're an API, not a user-facing error HTML page.
# we need to define what those error objects look like and how we throw them.
# XXX using Express's dev error handler for now.
app.use express.errorHandler()


## ROUTES:

app.get '/', (req, res, _) ->
  res.send 'ZoomHub'

app.get '/health', (req, res, _) ->
  res.send 'up'

app.get '/content/:id', (req, res, _) ->
  id = parseInt req.params.id, 10
  if not id? or isNaN id
    return res.json 404, error:
      code: 404
      message: 'Not found'

  content = Content.getById id, _
  if not content?
    return res.json 404, error:
      code: 404
      message: 'Not found'
  res.json 200, content

app.get /^\/https?:\/\/.+/, (req, res, _) ->
  url = req.url[1..]
  if not url?
    return res.json 400, error:
      message: 'Missing URL'

  content = Content.getByURL url, _
  if content?
    return res.redirect content.shareUrl

  content = Content.fromURL url, _
  # Redirect to metadata
  res.redirect content.self

  # Fetch source
  source = fetcher.fetch content, _

  # Create DZI
  destination = processor.process source, _

app.get '/:id', (req, res, _) ->
  id = parseInt req.params.id, 10
  if not id? or isNaN id
    return res.send 404
  content = Content.getById id, _
  if not content?
    return res.send 404
  res.render 'view', {content}


## MAIN:

if module is require.main
  app.listen config.PORT
  console.log "ZoomHub running at #{config.BASE_URL}"
else
  module.exports = app
