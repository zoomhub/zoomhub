## Proposal: ZoomHub API v2

Here are the breaking changes and open questions for version 2 of the API.

### Formats

With all modern browsers supporting CORS, get rid of JSONP and the non-RESTful
response format?

### Objects

**All** objects:

- Add a `self` (`selfURL`? `apiURL`?) property that points to the canonical
  API URL for this object.

- Add a `type` property that conveys what type of object this is?
  That might conflict with the proposed `type` property on **Content** objects;
  see below.

**Content** objects: (TODO: Rename to `Post`)

- Add an `error` property for semantic failure codes/reasons.
  (We were routinely asked for this.)

- Remove the `ready` and `failed` boolean properties; it’s equivalent --
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
  If there’s no strong benefit in doing so, maybe not worth it?

**Error** objects would be nicer than plaintext messages:

- `code` (string; semantic code that’ll be documented here)
- `message` (string; developer-facing to help in debugging, NOT user-facing)
- `data` (arbitrary and optional; will be documented w/ code if it’s needed)

### Routes

All routes would begin with `/v2` instead of `/v1`.
