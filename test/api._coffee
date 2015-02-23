#
# General API tests.
#
# TODO: We should have a separate test suite for properly testing the image
# submission flow, e.g. new --> queued --> progress --> ready or failed.
# That could/should cover both the UI and API since states are transient.
# That should also test that fetching the actual image tiles works too.
#

app = (require 'supertest')(require '../app')
{AssertionError, expect} = require 'chai'
ids = require './fixtures/ids'
Request = require 'request'
urls = require './fixtures/urls'
VM = require 'vm'


## Helpers

TYPE_JS = 'text/javascript; charset=utf-8'
TYPE_JSON = 'application/json; charset=utf-8'
TYPE_TEXT = 'text/plain; charset=utf-8'
TYPE_XML = 'application/xml'

TYPE_IMAGES =
    png: 'image/png'
    jpg: 'image/jpeg'
    jpeg: 'image/jpeg'

# Matches e.g. '/v1/content/Abc123' and captures the ID:
CONTENT_BY_ID_REGEX = /// ^/v1/content/ (\w+) $ ///

# We'll set this to a converted DZI object when we get one from the API:
DZI_CONVERTED = null

#
# Asserts that the given actual *text string* is a valid JSONP response,
# wrapping a Response info object in a call to the given function name.
#
# May also optionally match the response with the given expected object
# (which may be partial).
#
expectJSONP = (act, callback, exp={}) ->
    # instead of testing the string via regex (which is brittle since e.g.
    # Express adds a typeof check before calling the callback function),
    # just do the "real" thing and evaluate the string as JS directly:
    act = VM.runInNewContext """
        function #{callback}(resp) {
            return resp;
        }

        #{act}
    """

    expectResponse act, exp

#
# Asserts that the given actual object is a valid Response info object,
# optionally matching the given expected object (which may be partial).
#
expectResponse = (act, exp={}) ->
    expect(act).to.be.an 'object'

    _expectNumberWithin act.status, 200, 599
    _expectStringMatching act.statusText, /.+/

    if act.redirectLocation
        _expectFlexibleURL act.redirectLocation

    # responses must contain one of `content`, `dzi`, or `error`:
    if act.content
        expectContent act.content, exp.content
        delete exp.content  # so it's not deep-equaled below
        expect(act).to.not.have.key 'dzi'
        expect(act).to.not.have.key 'error'
    else if act.dzi
        expect(act).to.not.have.key 'content'
        expectDzi act.dzi, exp.dzi
        delete exp.dzi  # so it's not deep-equaled below
        expect(act).to.not.have.key 'error'
    else if act.error
        expect(act).to.not.have.key 'content'
        expect(act).to.not.have.key 'dzi'
        _expectStringMatching act.error, /.+/
    else
        throw new AssertionError "
            Expected response to have one of `content`, `dzi`, or `error`;
            none found: #{JSON.stringify act, null, 4}
        "

    _expectPartial act, exp

#
# Asserts that the given actual object is a valid Content info object,
# optionally matching the given expected object (which may be partial).
#
expectContent = (act, exp={}) ->
    expect(act).to.be.an 'object'

    _expectStringMatching act.id, /\w+/
    _expectAbsoluteURL act.url

    for prop in ['ready', 'failed']
        expect(act[prop]).to.be.a 'boolean'

    _expectNumberWithin act.progress, 0, 1

    # TODO: Do we want to test the actual makeup of these properties?
    # E.g. that the share URL is to the website with the same ID, etc.
    _expectAbsoluteURL act.shareUrl
    _expectStringMatching act.embedHtml, /// ^<script .+></script>$ ///

    # content should either be `ready`, `failed`, or neither:
    if act.ready or act.dzi     # both should be true together
        expect(act.ready).to.equal true
        expect(act.failed).to.equal false
        expect(act.progress).to.equal 1
        expectDZI act.dzi, exp.dzi
        delete exp.dzi  # so it's not deep-equaled below
    else if act.failed
        expect(act.ready).to.equal false
        expect(act.failed).to.equal true
        expect(act).to.not.have.key 'dzi'
    else
        expect(act.ready).to.equal false
        expect(act.failed).to.equal false
        expect(act).to.not.have.key 'dzi'

    _expectPartial act, exp

#
# Asserts that the given actual object is a valid DZI info object,
# optionally matching the given expected object (which may be partial).
#
expectDZI = (act, exp={}) ->
    expect(act).to.be.an 'object'

    _expectAbsoluteURL act.url

    for prop in ['width', 'height', 'tileSize', 'tileOverlap']
        _expectNumberWithin act[prop], 0, Infinity

    _expectStringMatching act.tileFormat, ///
        ^ ( #{(Object.keys TYPE_IMAGES).join '|'} ) $
    /// # e.g. ^(png|jpg|jpeg)$

    _expectPartial act, exp

#
# Asserts that the given actual object matches the given expected object,
# which may be partial. IOW, the actual object must be a superset.
#
# Regex values may also be given; since regex isn't a JSON type, it will be
# used to assert that the property is a string that matches the given regex.
#
_expectPartial = (act, exp={}) ->
    expect(act).to.be.an 'object'
    for prop of exp
        if exp[prop] instanceof RegExp
            _expectStringMatching act[prop], exp[prop]
        else
            expect(act[prop]).to.eql exp[prop]

_expectNumberWithin = (val, min, max) ->
    expect(val).to.be.a 'number'
    expect(val).to.be.within min, max

_expectStringMatching = (val, regex) ->
    expect(val).to.be.a 'string'
    expect(val).to.match regex

_expectAbsoluteURL = (val) ->
    _expectStringMatching val, /// ^https?://.+ ///

_expectFlexibleURL = (val) ->
    _expectStringMatching val, /// ^(https?://)?.+ ///


## Tests

describe 'API /v1/content', ->

    describe 'List', ->

        it 'should be interpreted as a get-by-URL, with no URL given', (_) ->
            app.get '/v1/content'
                .expect 400
                .expect 'Content-Type', TYPE_TEXT
                .expect /Missing/i
                .end _

    describe 'Get by URL', ->

        it 'should reject empty URLs', (_) ->
            app.get '/v1/content?url='
                .expect 400
                .expect 'Content-Type', TYPE_TEXT
                .expect /Missing/i
                .end _

        it 'should reject malformed URLs', (_) ->
            app.get "/v1/content?url=#{encodeURIComponent urls.MALFORMED}"
                .expect 400
                .expect 'Content-Type', TYPE_TEXT
                .expect /full URL/i
                .end _

        it 'should reject URLs w/out protocol', (_) ->
            app.get "/v1/content?url=#{encodeURIComponent urls.NO_PROTOCOL}"
                .expect 400
                .expect 'Content-Type', TYPE_TEXT
                .expect /full URL/i
                .end _

        it 'should reject non-HTTP URLs', (_) ->
            app.get "/v1/content?url=#{encodeURIComponent urls.NON_HTTP}"
                .expect 400
                .expect 'Content-Type', TYPE_TEXT
                .expect /full URL/i
                .end _

        # TEMP
        it 'should reject new HTTP URLs for now', (_) ->
            app.get "/v1/content?url=#{encodeURIComponent urls.randomize urls.IMAGE_NEW}"
                .expect 503
                .expect 'Content-Type', TYPE_TEXT
                .expect /Please wait/i
                .end _

        # TODO: Need reliable existing image across local dev envs.
        it 'should redirect existing (converted) HTTP URLs to ID', (_) ->
            resp = app.get "/v1/content?url=#{encodeURIComponent urls.IMAGE_CONVERTED}"
                .redirects 0
                .expect 301
                .expect 'Location', CONTENT_BY_ID_REGEX
                .expect 'Content-Type', TYPE_JSON
                .end _

            id = (resp.headers.location.match CONTENT_BY_ID_REGEX)[1]
            expect(id).to.equal ids.IMAGE_CONVERTED

            # the response body should also be the info for convenience:
            expectContent resp.body,
                id: ids.IMAGE_CONVERTED
                url: urls.IMAGE_CONVERTED

            expect(resp.body.ready).to.equal true
            # expectContent takes care of expecting `dzi`, etc., in this case.

    describe 'Get by ID', ->

        # TODO: Need reliable existing image across local dev envs.
        it 'should return info for existing (converted) image', (_) ->
            resp = app.get "/v1/content/#{ids.IMAGE_CONVERTED}"
                .expect 200
                .expect 'Content-Type', TYPE_JSON
                .end _

            expectContent resp.body,
                id: ids.IMAGE_CONVERTED
                url: urls.IMAGE_CONVERTED

            expect(resp.body.ready).to.equal true
            # expectContent takes care of expecting `dzi`, etc., in this case.

            # Save the returned DZI object, for us to test further below:
            DZI_CONVERTED = resp.body.dzi

        it 'should return 404 for non-existent image', (_) ->
            app.get '/v1/content/99999999'
                .expect 404
                .expect 'Content-Type', TYPE_TEXT
                .expect /No content/i
                .end _

    describe 'Non-RESTful', ->

        # TODO: Need reliable existing image across local dev envs.
        it 'should properly wrap 200 success responses', (_) ->
            {body} = app.get "/v1/content/#{ids.IMAGE_CONVERTED}?format=json"
                .expect 200
                .expect 'Content-Type', TYPE_JSON
                .end _

            expectResponse body,
                status: 200
                statusText: /OK/i
                content:
                    id: ids.IMAGE_CONVERTED
                    url: urls.IMAGE_CONVERTED

        it 'should properly wrap 400 error responses', (_) ->
            {body} = app.get '/v1/content?url=&format=json'
                .expect 200
                .expect 'Content-Type', TYPE_JSON
                .end _

            expectResponse body,
                status: 400
                statusText: /Bad Request/i
                error: /Missing/i

    describe 'JSONP', ->

        it 'should properly return callback JS', (_) ->
            rand = "#{Math.random()}"[2..]
            callback = "_#{rand}"

            {text} = app.get "/v1/content?url=&callback=#{callback}"
                .expect 200
                .expect 'Content-Type', TYPE_JS
                .end _

            expectJSONP text, callback,
                status: 400
                statusText: /Bad Request/i
                error: /Missing/i

    describe 'CORS', ->

        it 'should allow all origins', (_) ->
            app.get '/v1/content?url=&format=json'
                .set 'Origin', 'http://example.com'
                .expect 200
                .expect 'Access-Control-Allow-Origin', '*'
                .end _

    describe '(Generated DZIs)', ->

        it 'should be downloadable when returned as ready', (_) ->
            {url, width, height, tileSize, tileOverlap, tileFormat} = DZI_CONVERTED

            resp = Request.get url, _

            {statusCode, headers, body} = resp

            expect(statusCode).to.equal 200
            expect(headers['content-type']).to.equal TYPE_XML

            # HACK: For simplicity, hardcoding the exact format of the XML:
            expect(body).to.be.a 'string'
            expect(body).to.equal """
                <?xml version="1.0" encoding="utf-8"?>
                <Image TileSize="#{tileSize}" Overlap="#{tileOverlap}"
                 Format="#{tileFormat}" ServerFormat="Default"
                 xmlns="http://schemas.microsoft.com/deepzoom/2009">
                <Size Width="#{width}" Height="#{height}" /></Image>
            """.replace /\n/g, ''

        it 'should have downloadable tiles too', (_) ->
            # For simplicity, just download the level 0 tile.
            # Guaranteed to be there for any size image, and even better,
            # we know internally it's the last tile to be generated.
            {url, tileFormat} = DZI_CONVERTED
            tileURL = url.replace /[.]\w+$/, "_files/0/0_0.#{tileFormat}"

            resp = Request.get tileURL, _

            {statusCode, headers, body} = resp

            expect(statusCode).to.equal 200
            expect(headers['content-type']).to.equal TYPE_IMAGES[tileFormat]

            # No simple way to test the actual image itself; not bothering.
