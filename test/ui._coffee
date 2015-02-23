#
# General UI tests.
#
# TODO: We should have a separate test suite for properly testing the image
# submission flow, e.g. new --> queued --> progress --> ready or failed.
# That could/should cover both the UI and API since states are transient.
# That should also test that fetching the actual image tiles works too.
#

app = (require 'supertest')(require '../app')
{expect} = require 'chai'
ids = require './fixtures/ids'
urls = require './fixtures/urls'


## Helpers

TYPE_JS = 'application/javascript; charset=utf-8'

# Matches e.g. '/Abc123' and captures the ID:
VIEW_PAGE_REGEX = /// ^/ (\w+) $ ///


## Tests

describe 'UI', ->

    describe 'Home page', ->

        it 'should return Zoom.it copy', (_) ->
            app.get '/'
                .expect 200
                .expect /Zoom\.it/
                .end _

    describe 'About page', ->

        # TEMP
        it 'should redirect to the homepage for now', (_) ->
            app.get '/pages/about'
                .redirects 0
                .expect 302
                .expect 'Location', '/'
                .end _

    describe 'Create page', ->

        # TEMP
        it 'should redirect to the homepage for now', (_) ->
            app.get '/pages/create'
                .redirects 0
                .expect 302
                .expect 'Location', '/'
                .end _

    describe 'URL handler', ->

        it 'should reject empty URLs', (_) ->
            app.get '/?url='
                .expect 400
                .expect /Missing/i
                .end _

        it 'should reject malformed URLs', (_) ->
            app.get "/?url=#{encodeURIComponent urls.MALFORMED}"
                .expect 400
                .expect /full URL/i
                .end _

        it 'should reject URLs w/out protocol', (_) ->
            app.get "/?url=#{encodeURIComponent urls.NO_PROTOCOL}"
                .expect 400
                .expect /full URL/i
                .end _

        it 'should reject non-HTTP URLs', (_) ->
            app.get "/?url=#{encodeURIComponent urls.NON_HTTP}"
                .expect 400
                .expect /full URL/i
                .end _

        # TEMP
        it 'should reject new HTTP URLs for now', (_) ->
            app.get "/?url=#{encodeURIComponent urls.randomize urls.IMAGE_NEW}"
                .expect 503
                .expect /Please wait/i
                .end _

        # TODO: Need reliable existing image across local dev envs.
        it 'should redirect existing (converted) HTTP URLs to view page', (_) ->
            resp = app.get "/?url=#{encodeURIComponent urls.IMAGE_CONVERTED}"
                .redirects 0
                .expect 301
                .expect 'Location', VIEW_PAGE_REGEX
                .end _

            id = (resp.headers.location.match VIEW_PAGE_REGEX)[1]
            expect(id).to.equal ids.IMAGE_CONVERTED

    describe 'View page', ->

        # TODO: Need reliable existing image across local dev envs.
        it 'should return viewer for existing (converted) image', (_) ->
            app.get "/#{ids.IMAGE_CONVERTED}"
                .expect 200
                .expect /// #{ids.IMAGE_CONVERTED} ///  # anywhere, e.g. <title>
                .end _

            # TODO: How do we assert that this view page actually has a viewer
            # with the image? Selenium?

        it 'should return 404 for non-existent image', (_) ->
            app.get '/99999999'
                .expect 404
                .expect /No content with ID/i
                .end _

    describe 'Embed', ->

        # TODO: Need reliable existing image across local dev envs.
        it 'should return JS for existing (converted) image', (_) ->
            app.get "/#{ids.IMAGE_CONVERTED}.js"
                .expect 200
                .expect 'Content-Type', TYPE_JS
                .expect /// #{ids.IMAGE_CONVERTED} ///  # anywhere
                .end _

            # TODO: How do we assert that this embed script actually works,
            # and shows this image? Selenium?

        # TEMP: We should be returning JS instead; known FIXME in the code.
        # Until that's fixed, testing our current behavior.
        it 'should return 404 for non-existent image', (_) ->
            app.get '/99999999.js'
                .expect 404
                .expect /No content with ID/i
                .end _
