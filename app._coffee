require 'newrelic'
config = require './config'
express = require 'express-streamline'
jade = require 'jade'
path = require 'path'
routes = require './lib/routes'


## APP:

app = express()

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
app.use config.STATIC_DIR, express.static config.STATIC_PATH

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

# Meta:
app.get '/health', routes.getHealth

# API: (TODO: These should be under an api vhost!)
app.get '/v1/content/:id', routes.getContentById
app.get '/v1/content', routes.getContentByURL   # expects ?url param

# UI:
app.get '/', routes.getHomepage     # this includes ?url support
app.get /^\/https?:\/\/.+/, (req, res, _) ->
    req.params.url = req.url[1..]   # req.url includes the query string
    routes.submitURL req, res, _
app.get '/:id.js', routes.getEmbed
app.get '/:id', routes.getViewer


## MAIN:

if module is require.main
    app.listen config.PORT
    console.log "ZoomHub running at #{config.BASE_URL}"
else
    module.exports = app
