#
# Known URLs on the web that we can reliably use for testing.
#


## Helpers

HTTP_REGEX = /^https?:\/\//

replaceProtocol = (url, str) ->
    url.replace HTTP_REGEX, str


## Public

@IMAGE = 'http://openseadragon.github.io/example-images/highsmith/09250_150px.jpg'

@WEBPAGE = 'http://www.example.com/'

@BINARY = 'http://httpbin.org/stream-bytes/1024'

@['404'] = 'http://httpbin.org/status/404'

@['500'] = 'http://httpbin.org/status/500'

@JSON = 'http://httpbin.org/get'

@MALFORMED = 'lasjdoasj)(¨‚Ô‚ˆÔ∏ŒÂ;sd)'

@NO_PROTOCOL = replaceProtocol @IMAGE, ''   # www.example.com/...

@NON_HTTP = replaceProtocol @IMAGE, 'ftp://'    # ftp://www.example.com/...

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
