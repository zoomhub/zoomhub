#
# Handlers for all of our various routes.
#

config = require '../config'
Content = require './content'
Embed = require './embed'
Fetcher = require './fetcher'
path = require 'path'
Processor = require './processor'

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
# - req.params.id
#
@getContentById = (req, res, _) ->
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


#
# Pre-reqs:
# - req.params.url
#
@getContentByURL = (req, res, _) ->
    # FIXME: This sometimes redirects to the view page; this route should
    # always return API data and redirect to the content API route.
    handleURL res, req.params.url, _


## UI:

#
# Pre-reqs: (none)
#
# TODO: Zoom.it back-compat of ?url query string support.
#
@getHomepage = (req, res, _) ->
    res.render 'home'

#
# Pre-reqs:
# - req.params.id
#
@getEmbed = (req, res, _) ->
    id = parseInt req.params.id, 10
    if not id? or isNaN id
        return res.send 404
    res.send embed.generate id, _


#
# Pre-reqs:
# - req.params.id
#
@getViewer = (req, res, _) ->
    id = parseInt req.params.id, 10
    if not id? or isNaN id
        return res.send 404
    content = Content.getById id, _
    if not content?
        return res.send 404
    res.render 'view', {content}


#
# Pre-reqs:
# - req.params.url
#
@submitURL = (req, res, _) ->
    # FIXME: This sometimes returns API JSON; this route should always
    # redirect to the view page.
    handleURL res, req.params.url, _


## HELPERS:

# Helper for the two different routes for URLs:
handleURL = (res, url, _) ->
    if not url?
        res.json 400, error:
            message: 'Please give us the full URL,
                including `http://` or `https://`.'
        return false

    content = Content.getByURL url, _
    if content?
        res.redirect content.shareUrl
        return false

    content = Content.fromURL url, _
    # Redirect to metadata
    res.redirect content.self

    # Async operations:

    # Fetch source
    source = fetcher.fetch content, _

    # Create DZI
    destination = processor.process source, _
