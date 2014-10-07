#
# Handlers for all of our various routes.
#

config = require '../config'
Content = require './content'
DZIParser = require './dziparser'
Embed = require './embed'
Errors = require './errors'
Fetcher = require './fetcher'
path = require 'path'
Processor = require './processor'
URL = require 'url'

# Constants
PIPELINE_PATH = path.join __dirname, '..', 'pipeline'

# Fetcher
FETCHER_PATH = path.join PIPELINE_PATH, 'fetcher'
fetcher = new Fetcher FETCHER_PATH

# Processor
DZI_PATH = path.join config.STATIC_PATH, config.DZI_DIR
processor = new Processor DZI_PATH

# Embed
embed = new Embed config.STATIC_PATH


## META:

@getHealth = (req, res, _) ->
    res.send 'up'


## API:

#
# Pre-reqs:
# - `id` param
#
@getContentById = (req, res, _) ->
    id = req.param 'id'
    if not id
        errorAPI res, 404, Errors.MISSING_ID_OR_URL
        return

    content = Content.getById id, _
    if not content?
        errorAPI res, 404, Errors.NO_CONTENT_WITH_ID.replace '{ID}', id
        return

    res.json 200, content

#
# Pre-reqs:
# - `url` param
#
@getContentByURL = (req, res, _) ->
    url = req.param 'url'
    if not url
        errorAPI res, 404, Errors.MISSING_ID_OR_URL
        return
    if not validateURL url
        errorAPI res, 400, Errors.MALFORMED_URL
        return

    # Get or create this content, enqueueing it for conversion if it's new.
    # But support us rejecting new content, e.g. if we're overloaded.
    #
    # TODO: This logic should probably be abstracted in the Content class,
    # since it's core business logic, used by (and currently duplicated by)
    # both our API and our website. But in order to do that, we'd want it to
    # throw semantic error codes, so that error handling can remain custom.
    #
    content = Content.getByURL url, _
    if not content
        if config.ALLOW_NEW_CONTENT
            content or= Content.createFromURL url, _
            enqueueForConversion content
        else
            errorAPI res, 503, Errors.SERVICE_UNAVAILABLE
            return

    # HACK: Hardcoding knowledge of URL from app._coffee.
    redirectAPI res, 301, "/v1/content/#{content.id}", content


## UI:

#
# Pre-reqs: (none)
#
# But supports passing a URL for conversion, via ?url query string param.
# This was part of Zoom.it's original UI / informal website API.
#
@getHomepage = (req, res, _) =>
    if req.query.url
        @submitURL req, res, _
    else
        res.render 'home'

#
# Pre-reqs:
# - `id` param
#
@getEmbed = (req, res, _) ->
    id = req.param 'id'
    if not id
        errorHTML res, 404, Errors.MISSING_ID_OR_URL
        return

    res.type 'application/javascript'
    res.send embed.generate id, _


#
# Pre-reqs:
# - `id` param
#
@getViewer = (req, res, _) ->
    id = req.param 'id'
    if not id
        errorHTML res, 404, Errors.MISSING_ID_OR_URL
        return

    content = Content.getById id, _
    if not content?
        errorHTML res, 404, Errors.NO_CONTENT_WITH_ID.replace '{ID}', id
        return

    res.render 'view', {content}


#
# Pre-reqs:
# - `url` param
#
@submitURL = (req, res, _) ->
    url = req.param 'url'
    if not url
        errorHTML res, 404, Errors.MISSING_ID_OR_URL
        return
    if not validateURL url
        errorHTML res, 400, Errors.MALFORMED_URL
        return

    # Get or create this content, enqueueing it for conversion if it's new.
    # But support us rejecting new content, e.g. if we're overloaded.
    #
    # TODO: This logic should probably be abstracted in the Content class,
    # since it's core business logic, used by (and currently duplicated by)
    # both our API and our website. But in order to do that, we'd want it to
    # throw semantic error codes, so that error handling can remain custom.
    #
    content = Content.getByURL url, _
    if not content
        if config.ALLOW_NEW_CONTENT
            content or= Content.createFromURL url, _
            enqueueForConversion content
        else
            errorHTML res, 503, Errors.SERVICE_UNAVAILABLE
            return

    # HACK: Hardcoding knowledge of URL from app._coffee.
    res.redirect 301, "/#{content.id}"


## HELPERS:

#
# Return an API error response with the given status code and message.
#
errorAPI = (res, status, message) ->
    res.status status
    res.type 'text/plain'
    res.send message

#
# Render an HTML error page with the given status code and message.
#
errorHTML = (res, status, message) ->
    # TODO: Render nice error view instead of this plaintext error message.
    res.send status, message

#
# Return a redirect API response with a JSON body for convenience.
#
redirectAPI = (res, status, location, body={}) ->
    res.status status
    res.set 'Location', location
    res.json body

#
# Returns whether the given URL is well-formed or not.
#
# TODO: This should probably be encapsulated in the Content class,
# as validation is core business logic, not just for our API layer.
#
validateURL = (url) ->
    # Use Node's built-in URL parser, and duck-type / feature-detect:
    uri = URL.parse url
    uri.protocol in ['http:', 'https:'] and !!uri.host and !!uri.path
        # TODO: Do we also want to reject e.g. localhost, etc.?
        # If we do that, we should differentiate via semantic error codes.

#
# Enqueues the given content for conversion.
#
# TODO: Should this be abstracted away in a more business logic layer?
# Rather than this file which defines API & website routes?
#
enqueueForConversion = (content) ->
    # TODO: We should properly use a queue and workers for conversion!
    # For now, converting directly on our web servers.
    convertContent content, (err) ->
        if err
            console.error "(Async) Error converting content!
                (ID: #{content.id}) #{err.stack or err}"
        else
            console.log "(Async) Successfully converted content.
                (ID: #{content.id})"

#
# Converts the given content.
#
# TODO: Should this be abstracted away in a more business logic layer?
# Rather than this file which defines API & website routes?
#
convertContent = (content, _) ->
    try
        source = fetcher.fetch content, _
        destination = processor.process source, _
        dzi = DZIParser.parse destination, _
        content.markReady dzi, _
    catch err
        content.markFailed _
        throw err
