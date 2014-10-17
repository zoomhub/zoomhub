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
urls = require './fixtures/urls'
VM = require 'vm'


## Helpers

TYPE_JS = 'text/javascript; charset=utf-8'
TYPE_JSON = 'application/json; charset=utf-8'
TYPE_TEXT = 'text/plain; charset=utf-8'

# Matches e.g. '/v1/content/Abc123' and captures the ID:
CONTENT_BY_ID_REGEX = /// ^/v1/content/ (\w+) $ ///

# This gets set to a known ID when we create or derive one:
EXISTING_ID = null

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
    if act.dzi
        expect(act).to.not.have.key 'content'
        expectDzi act.dzi, exp.dzi
        delete exp.dzi  # so it's not deep-equaled below
        expect(act).to.not.have.key 'error'
    if act.error
        expect(act).to.not.have.key 'content'
        expect(act).to.not.have.key 'dzi'
        _expectStringMatching act.error, /.+/

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

    if act.ready or act.dzi
        expectDZI act.dzi, exp.dzi
        delete exp.dzi  # so it's not deep-equaled below

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

    _expectStringMatching act.tileFormat, /^(png|jpeg)$/

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
            app.get "/v1/content?url=#{encodeURIComponent urls.randomize urls.IMAGE}"
                .expect 503
                .expect 'Content-Type', TYPE_TEXT
                .expect /Please wait/i
                .end _

        # TODO: Need reliable existing image across local dev envs.
        it.skip 'should redirect existing HTTP URLs to view page', (_) ->
            resp = app.get "/v1/content?url=#{encodeURIComponent urls.IMAGE}"
                .expect 301
                .expect 'Location', CONTENT_BY_ID_REGEX
                .expect 'Content-Type', TYPE_JSON
                .end _

            EXISTING_ID = (resp.headers.location.match CONTENT_BY_ID_REGEX)[1]

            # the response body should also be the info for convenience:
            expectContent resp.body,
                id: EXISTING_ID
                url: urls.IMAGE

    describe 'Get by ID', ->

        # TODO: Need reliable existing image across local dev envs.
        it.skip 'should return viewer for existing image', (_) ->
            resp = app.get "/v1/content/#{EXISTING_ID}"
                .expect 200
                .expect 'Content-Type', TYPE_JSON
                .end _

            expectContent resp.body,
                id: EXISTING_ID

        it 'should return 404 for non-existent image', (_) ->
            app.get '/v1/content/99999999'
                .expect 404
                .expect 'Content-Type', TYPE_TEXT
                .expect /No content/i
                .end _

    describe 'Non-RESTful', ->

        # TODO: Need reliable existing image across local dev envs.
        it.skip 'should properly wrap 200 success responses', (_) ->
            {body} = app.get "/v1/content/#{EXISTING_ID}?format=json"
                .expect 200
                .expect 'Content-Type', TYPE_JSON
                .end _

            expectResponse body,
                status: 200
                statusText: /OK/i
                content:
                    id: EXISTING_ID

        # TODO: Need reliable existing image across local dev envs.
        it.skip 'should properly wrap 301 redirect responses', (_) ->
            {body} = app.get "/v1/content\
                    ?url=#{encodeURIComponent urls.IMAGE}&format=json"
                .expect 200
                .expect 'Content-Type', TYPE_JSON
                .end _

            expectResponse body,
                status: 301
                statusText: /Moved Permanently/i
                redirectLocation: CONTENT_BY_ID_REGEX
                content:
                    url: urls.IMAGE

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
