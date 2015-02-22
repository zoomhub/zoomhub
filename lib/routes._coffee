#
# Handlers for all of our various routes.
#

config = require '../config'
Content = require './content'
DZIParser = require './dziparser'
Embed = require './embed'
Errors = require './errors'
Fetcher = require './fetcher'
http = require 'http'
path = require 'path'
Processor = require './processor'
URL = require 'url'

# Constants
PIPELINE_PATH = path.join __dirname, '..', 'pipeline'

# Fetcher
FETCHER_PATH = path.join PIPELINE_PATH, 'fetcher'
fetcher = new Fetcher FETCHER_PATH

# Processor
DZI_DIR_PATH = path.join config.STATIC_FILE_PATH, config.DZI_SUBDIR_PATH
processor = new Processor DZI_DIR_PATH


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
        return respondAPI res,
            status: 404
            error: Errors.MISSING_ID_OR_URL

    content = Content.getById id, _
    if not content?
        return respondAPI res,
            status: 404
            error: Errors.NO_CONTENT_WITH_ID.replace '{ID}', id

    respondAPI res, {status: 200, content}

#
# Pre-reqs:
# - `url` param
#
@getContentByURL = (req, res, _) ->
    url = req.param 'url'
    if not url
        return respondAPI res,
            status: 400
            error: Errors.MISSING_ID_OR_URL
    if not validateURL url
        return respondAPI res,
            status: 400
            error: Errors.MALFORMED_URL

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
            return respondAPI res,
                status: 503
                error: Errors.SERVICE_UNAVAILABLE

    # HACK: Hardcoding knowledge of URL from app._coffee.
    respondAPI res,
        status: 301
        redirect: "/v1/content/#{content.id}"
        content: content


## UI:

#
# Pre-reqs: (none)
#
# But supports passing a URL for conversion, via ?url query string param.
# This was part of Zoom.it's original UI / informal website API.
#
@getHomePage = (req, res, _) =>
    if req.query.url?
        @submitURL req, res, _
    else
        res.render 'home'

#
# Pre-reqs: (none)
#
# This is for various Zoom.it content pages, e.g. create, about, FAQ, etc.
#
@getContentPage = (req, res, _) ->
    # TEMP: Since we haven't implemented any of these pages, just redirect.
    # Our logs will tell us which pages are getting requested.
    res.redirect '/'

#
# Pre-reqs:
# - `id` param
#
@getEmbed = (req, res, _) ->
    id = req.param 'id'
    if not id
        errorHTML res, 404, Errors.MISSING_ID_OR_URL
        return

    content = Content.getById id, _
    if not content?
        errorHTML res, 404, Errors.NO_CONTENT_WITH_ID.replace '{ID}', id
        return

    res.type 'application/javascript'
    res.send Embed.generate content, _, req.query


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
        # FIXME: We should still be returning the embed JS here;
        # it should just display this error message.
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
        errorHTML res, 400, Errors.MISSING_ID_OR_URL
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
# Returns whether the given request seeks a RESTful or non-RESTful response.
#
isRESTful = (req) ->
    # We're RESTful by default, so unless this is a non-RESTful one...
    # (TODO: Should we be returning an error on unrecognized ?formats?)
    not (req.query.format or req.query.callback)

#
# Helper to send API responses in the requested RESTful or non-RESTful format.
#
respondAPI = (res, props={}) ->
    {status, redirect, error, content, dzi} = props

    if isRESTful res.req
        res.status status
        res.set 'Location', redirect if redirect

        if error
            res.type 'text/plain'
            res.send error
        else
            res.json content or dzi

    else
        # our public API specifies a `statusText` prop:
        props.statusText = http.STATUS_CODES[status]

        # our public API calls it `redirectLocation`, a little less elegant:
        if redirect
            props.redirectLocation = redirect
            delete props.redirect

        # support JSONP ?callback in the non-RESTful mode:
        res.jsonp props

    return  # explicitly a void method

#
# Render an HTML error page with the given status code and message.
#
errorHTML = (res, status, message) ->
    # TODO: Render nice error view instead of this plaintext error message.
    res.send status, message

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
