# API

## v1 (Zoom.it)

Here was the Zoom.it API ([archived docs][zoomit-api-docs]),
for as much back-compat as we want to retain.

### Formats

The API returns data in **JSON** format.
***XML is no longer supported.*** *Sorry!*

The API supports both a **RESTful** and **non-RESTful** response format.
([Zoom.it archived docs on this][zoomit-api-formats])

- RESTful means that we'll return proper status codes (e.g. 404, 301) and
  return resources directly.

- Non-RESTful means that we'll always return a 200 status code, and wrap our
  resources in a "response" object.

The API is RESTful by default. To specify non-RESTful, add `?format=json` to
API URLs.

The API also supports **[JSONP](http://en.wikipedia.org/wiki/JSONP)**,
which is always returned in the non-RESTful format.
To request JSONP, add a `?callback` parameter to API URLs.

***TODO:*** *We need to implement both the non-RESTful format and JSONP.*

***TODO:*** *We should also add CORS support, as an alternative to JSONP.*

### Objects

**DZI** objects have the form:

- `url` (string; the URL to the DZI; should *not* be cached/saved)
- `width` (int; ***TODO***)
- `height` (int; ***TODO***)
- `tileSize` (int; ***TODO***)
- `tileOverlap` (int; ***TODO***)
- `tileFormat` (string; ***TODO***)

**Content** objects have the form:

- `id` (string; the ZoomHub ID; case-sensitive alphanumeric)
- `url` (string; the original source URL)
- `ready` (boolean)
- `failed` (boolean)
- `progress` (double; between 0 and 1; ***NOTE:*** *This isn't actually wired up yet.*)
- `shareUrl` (string; the URL for viewing)
- `embedHtml` (string; HTML snippet for this content's embeddable viewer)
- `title` (optional string; ***TODO?***)
- `attributionText` (optional string; ***TODO?***)
- `attributionUrl` (optional string; ***TODO?***)
- `dzi` (optional DZI object; null if still in progress or failed)

**Response** objects (for non-RESTful API requests) have the form:

- `status` (int; the HTTP status code of the response)
- `statusText` (string; the human-readable name of the HTTP status code)
- `redirectLocation` (optional string; for 3xx redirect responses)
- `retryAfter` (optional int; ***TODO?***)
- `content` (optional Content object; on successful content requests)
- `dzi` (optional DZI object; on successful DZI requests)
- `error` (optional string; on failed requests)

Note that all response objects have a `content`, `dzi`, or `error` object.

### Routes

`GET /v1/content/:id`

- 200 w/ Content object if exists
- 404 w/ error message if doesn't exist

`GET /v1/content?url=<url>` (be sure to percent-encode the URL)

- 3xx to `/v1/content/:id`, w/ Content object for convenience, if URL is valid
- 400 w/ error message if URL is invalid (e.g. malformed)
- 503 w/ error message if URL is new and ZoomHub is closed for new content

`GET /v1/dzi/:id` (***TODO***)

- 3xx to DZI URL, w/ DZI object for convenience, if exists and ready
- 404 w/ `Retry-After` header and message body if not ready but in progress
- 404 w/ error message if not found or failed

`GET /v1/dzi?url=<url>` (***TODO***)

- 3xx to DZI URL, w/ DZI object for convenience, if successful and ready
- 3xx to `/v1/dzi/:id` if not ready or failed, but URL is valid
- 400 w/ error message if URL is invalid (e.g. malformed)
- 503 w/ error message if URL is new and ZoomHub is closed for new content

### Inputs

Beyond simple image files (JPEG, PNG, etc.), Zoom.it used to support a large
variety of other input types:

- SVG images
- PDF files
- Web pages (with special support for certain sites, e.g. Flickr)

***TODO:*** *Add support for these things again.*


## v2 (WIP)

Here are the breaking changes and open questions for v2 of the API.

### Formats

With all modern browsers supporting CORS, get rid of JSONP and the non-RESTful
response format?

### Objects

**All** objects:

- Add a `self` (`selfURL`? `apiURL`?) property that points to the canonical
  API URL for this object.

- Add a `type` property that conveys what type of object this is?
  That might conflict with the proposed `type` property on Content objects;
  see below.

**Content** objects:

- Add an `error` property for semantic failure codes/reasons.
  (We were routinely asked for this.)

- Remove the `ready` and `failed` boolean properties; it's equivalent --
  but more robust -- to just check for `dzi` and `error` directly.

- Rename `url` to `sourceURL`, to disambiguate from our other URLs?
  But this should be consistent with the request param (e.g. query string).

- Properly case `shareUrl` and `embedHtml` to `shareURL` and `embedHTML`?
  And/or rename `shareURL` to `wwwURL`?

- Add a `type` property that conveys whether this content converted to a DZI
  or a DZC? Or would it be better to just add a `dzc` property and let the
  client check both/either, like `error`?

- If we ever generate both a DZI and a DZC for certain content (e.g. a Flickr
  album link), would it be good to namespace `error` and `progress` to e.g.
  `dziError` and `dziProgress`?

**DZI** objects:

- Should we move to the OpenSeadragon JSON format?
  (A straight-up XML-to-JSON representation of the DZI XML.)
  If there's no strong benefit in doing so, maybe not worth it?

**Error** objects would be nicer than plaintext messages:

- `code` (string; semantic code that'll be documented here)
- `message` (string; developer-facing to help in debugging, NOT user-facing)
- `data` (arbitrary and optional; will be documented w/ code if it's needed)

### Routes

All routes would begin with `/v2` instead of `/v1`.



[zoomit-api-docs]: https://web.archive.org/web/20140814051321/http://zoom.it/pages/api/
[zoomit-api-formats]: https://web.archive.org/web/20140702025809/http://zoom.it/pages/api/formats/rest-vs-non-rest
