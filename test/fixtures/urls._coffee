#
# Known URLs on the web that we can reliably use for testing.
#


## Helpers

HTTP_REGEX = /^https?:\/\//

replaceProtocol = (url, str) ->
    url.replace HTTP_REGEX, str


## Public

# TEMP: Using URLs that we *know* we have (even locally) or don't have.
# Ideally, we wouldn't rely on external URLs for our tests, but we have to for
# now since we don't have new URL acceptance hooked up yet.
@IMAGE_NEW =
    'http://openseadragon.github.io/example-images/highsmith/09250_150px.jpg'
@IMAGE_CONVERTED =
    'http://media.stenaline.com/media_SE/lalandia-map-zoomit/lalandia-map.jpg'
@IMAGE_QUEUED =
    'http://wdh.blob.core.windows.net/deepzoom-sources/hampi/HampiCB/02-Lakshmi,%20The%20Temple%20Elephant/09-Lakshmi%20and%20little%20girl.jpg'
# TODO: IMAGE_FAILED? Etc.

@WEBPAGE = 'http://www.example.com/'

@BINARY = 'http://httpbin.org/stream-bytes/1024'

@['404'] = 'http://httpbin.org/status/404'

@['500'] = 'http://httpbin.org/status/500'

@JSON = 'http://httpbin.org/get'

@MALFORMED = 'lasjdoasj)(¨‚Ô‚ˆÔ∏ŒÂ;sd)'

@NO_PROTOCOL = replaceProtocol @IMAGE_NEW, ''   # www.example.com/...

@NON_HTTP = replaceProtocol @IMAGE_NEW, 'ftp://'    # ftp://www.example.com/...

#
# Helper to randomize the given URL, to generate a new (unseen) one.
#
@randomize = (url) ->
    sep = if (url.indexOf '?') >= 0 then '&' else '?'
    rand = "#{Math.random()}"[2..]

    "#{url}#{sep}__rand=#{rand}"

#
# Returns a URL that redirects to the given one.
#
@getRedirectTo = (url) ->
    "http://httpbin.org/redirect-to?url=#{encodeURIComponent url}"
