config = require './config'
deepzoom = require 'deepzoomtools'
express = require 'express'
fs = require 'fs'
jade = require 'jade'
path = require 'path'
request = require 'request'
tiler = require './src/tiler'


ID = 0
PRIVATE_PATH = path.join __dirname, 'private'
STATIC_PATH = path.join __dirname, 'public'
STATIC_URL = '/static'


app = require('streamline-express') express()

# Template engine
app.engine 'jade', jade.__express
app.set 'view engine', 'jade'


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
    setTimeout _, 500   # mimic async operation
    res.json 'Hello world.'

app.get '/content', (req, res, _) ->
    url = req.query?.url
    if not url?
        return res.json 400, {error: "Missing URL"}
    id = ++ID
    contentPath = path.join STATIC_PATH, "#{id}.jpg"
    writer = request(url).pipe fs.createWriteStream contentPath
    writer.on 'error', (err) ->
        return res.json 500, {error: err?.stack or err}
    writer.on 'finish', (_) ->
        # TODO we need to explicitly catch errors here since the 'finish'
        # handler isn't *actually* an async handler, so errors we "throw"
        # here won't propagate to an error response. how to fix? domains?
        try
            deepzoom.create contentPath, _
            return res.json 200, {id, url}
        catch err
            return res.json 500, {error: err?.stack or err}

app.get '/tiler-test', (req, res, _) ->
    imagePath = path.join PRIVATE_PATH, '1.jpg'
    try
        tiler.start imagePath, _
        res.json 200, {success: true}
    catch err
        res.json 500, {error: err?.stack or err}

app.get '/:id', (req, res, _) ->
    id = req.param 'id'
    res.render 'view', {id}



## MAIN:

if module is require.main
    app.listen config.PORT
    console.log """
        Zooming service listening at http://localhost:#{config.PORT}/ ...
    """
else
    module.exports = app
