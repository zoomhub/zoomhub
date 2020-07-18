# ZoomHub API

The following documents the current state of the ZoomHub API which strives to
retain as much backward compatibility with the original
[Zoom.it API][zoomit-api-docs] (`/v1/*`) as possible. Please check the
<a href="#breaking-changes">**Breaking Changes**</a> section for features that
have been discontinued due to either lack of use, lack of developer resources,
or both.

### Format

The API returns data serialized as **JSON** and generally follows **RESTful**
conventions:

- Return semantic HTTP status codes, e.g. `301 Moved Permanently`,
  `404 Not Found`

- Return resources directly in the body, e.g. `{"id": "4rcn", …}` as opposed to
  `{"response": {"id": "4rcn", …}}`.

The API also supports **[JSONP]**, which is always returned in a non-RESTful
format. To request JSONP, add a `?callback` parameter to API URLs. Non-RESTful
means the API always returns a `200 OK` HTTP status code and it wraps resources
in a `response` JSON container object. This feature was built to support certain
limited API clients such as browsers, Silverlight, and Flash.

### Objects

**DZI** objects have the form:

- `url` (string; the URL to the DZI; **MUST NOT** be cached nor saved)
- `width` (int)
- `height` (int)
- `tileSize` (int)
- `tileOverlap` (int)
- `tileFormat` (string; currently supported: `jpg` or `png`)

**Content** objects have the form:

- `id` (string; the ZoomHub ID; case-sensitive alphanumeric)
- `url` (string; the original source URL)
- `ready` (boolean)
- `failed` (boolean)
- `progress` (double; between 0 and 1;
  **NOTE:** Currently only `0` or `1` are implemented.)
- `shareUrl` (string; the URL for viewing)
- `embedHtml` (string; HTML snippet for this content’s embeddable viewer)
- `title` (optional string; **_TODO?_**)
- `attributionText` (optional string; **_TODO?_**)
- `attributionUrl` (optional string; **_TODO?_**)
- `dzi` (optional DZI object; `null` if `progress < 1.0` or `failed === true`)

**Response** objects (for non-RESTful API requests) have the form:

- `status` (int; the HTTP status code of the response)
- `statusText` (string; the human-readable name of the HTTP status code)
- `redirectLocation` (optional string; for `3xx` redirect responses)
- `retryAfter` (optional int; **_TODO?_**)
- `content` (optional Content object; on successful content requests)
- `dzi` (optional DZI object; on successful DZI requests)
- `error` (optional string; on failed requests)

Note that all response objects have a `content`, `dzi`, or `error` object.

### URLs

`POST /v1/content`

- 200 w/ If content was uploaded successfully

`GET /v1/content/:id`

- 200 w/ Content object if exists
- 404 w/ error message if doesn’t exist

`GET /v1/content?url=<url>` (be sure to percent-encode the URL)

- 3xx to `/v1/content/:id`, w/ Content object for convenience, if URL is valid
- 400 w/ error message if URL is invalid (e.g. malformed)
- 503 w/ error message if URL is new and ZoomHub is closed for new content

`GET /v1/dzi/:id` (**_TODO_**)

- 3xx to DZI URL, w/ DZI object for convenience, if exists and ready
- 404 w/ `Retry-After` header and message body if not ready but in progress
- 404 w/ error message if not found or failed

`GET /v1/dzi?url=<url>` (**_TODO_**)

- 3xx to DZI URL, w/ DZI object for convenience, if successful and ready
- 3xx to `/v1/dzi/:id` if not ready or failed, but URL is valid
- 400 w/ error message if URL is invalid (e.g. malformed)
- 503 w/ error message if URL is new and ZoomHub is closed for new content

### Input Types

Beyond simple image files (JPEG, PNG, etc.), Zoom.it used to support a large
variety of other input types:

- SVG images
- PDF files
- Web pages (with special support for certain sites, e.g. Flickr)

**_TODO:_** _Add support for these input types again._

<a name="breaking-changes"></a>

## Breaking Changes

- The following Zoom.it API features have been discontinued in the ZoomHub API
  due to lack of use by external API clients (based on server logs from
  November 1, 2015 until March 1, 2016) and lack of developer resources:

  - XML as output format (opt-in using `?format=xml`).
  - Non-RESTful response format without JSONP, e.g. using just `?format=json`
    without `callback` query parameter.

[jsonp]: http://en.wikipedia.org/wiki/JSONP
[zoomit-api-docs]: https://web.archive.org/web/20140814051321/http://zoom.it/pages/api/
[zoomit-api-formats]: https://web.archive.org/web/20140702025809/http://zoom.it/pages/api/formats/rest-vs-non-rest
