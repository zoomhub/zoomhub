#
# General API tests.
#
# TODO: We should have a separate test suite for properly testing the image
# submission flow, e.g. new --> queued --> progress --> ready or failed.
# That could/should cover both the UI and API since states are transient.
# That should also test that fetching the actual image tiles works too.
#

app = (require 'supertest')(require '../app')
{expect} = require 'chai'
urls = require './fixtures/urls'


## Helpers

TYPE_JSON = 'application/json'
TYPE_TEXT = 'text/plain; charset=utf-8'

# Matches e.g. '/v1/content/Abc123' and captures the ID:
CONTENT_BY_ID_REGEX = /// ^/v1/content/ (\w+) $ ///

# This gets set to a known ID when we create or derive one:
EXISTING_ID = null

#
# Asserts that the given actual object is a valid Content info object,
# optionally matching the given expected object (which may be partial).
#
expectContent = (act, exp={}) ->
    expect(act).to.be.an 'object'

    _expectStringMatching act.id, /\w+/
    _expectURL act.url

    for prop in ['ready', 'failed']
        expect(act[prop]).to.be.a 'boolean'

    expect(act.progress).to.be.a 'number'
    expect(act.progress).to.be.within 0, 1

    # TODO: Do we want to test the actual makeup of these properties?
    # E.g. that the share URL is to the website with the same ID, etc.
    _expectURL act.shareUrl
    _expectStringMatching act.embedHtml, /// ^<script .+></script>$ ///

    if act.ready or act.dzi
        expectDZI act.dzi

    _expectPartial act, exp

#
# Asserts that the given actual object is a valid DZI info object,
# optionally matching the given expected object (which may be partial).
#
expectDZI = (act, exp={}) ->
    expect(act).to.be.an 'object'

    _expectURL act.url

    for prop in ['width', 'height', 'tileSize', 'tileOverlap']
        expect(act[prop]).to.be.a 'number'
        expect(act[prop]).to.be.greaterThan 0

    _expectStringMatching act.tileFormat, /^(png|jpeg)$/

    _expectPartial act, exp

#
# Asserts that the given actual object matches the given expected object,
# which may be partial. IOW, the actual object must be a superset.
#
_expectPartial = (act, exp={}) ->
    expect(act).to.be.an 'object'
    for prop of exp
        expect(act[prop]).to.eql exp[prop]

_expectStringMatching = (val, regex) ->
    expect(val).to.be.a 'string'
    expect(val).to.match regex

_expectURL = (val) ->
    _expectStringMatching val, /// ^https?://.+ ///


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
