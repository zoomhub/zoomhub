config = require '../config'
crypto = require 'crypto'
redis = require 'redis'


# Redis
client = redis.createClient()

# Keys
HASH_ALGORITHM = 'sha256'
HASH_ENCODING = 'hex'

NEXT_ID_KEY = 'content:next.id'

getIdKey = (id) ->
  "content:id:#{id}"

getURLKey = (url) ->
  hash = crypto.createHash HASH_ALGORITHM
  hash.update url
  digest = hash.digest HASH_ENCODING
  "content:url:#{digest}"


# Public API
module.exports = class Content
  constructor: (@id, url) ->
    @self = "#{config.BASE_URL}/content/#{@id}"
    @urls =
      source: url
      view: "#{config.BASE_URL}/#{@id}"
    @type = 'dzi'


  @getById: (id, _) ->
    key = getIdKey id
    result = client.get key, _
    JSON.parse result

  @getByURL: (url, _) ->
    id = client.get getURLKey(url), _
    if not id?
      return null
    @getById id, _

  @fromURL: (url, _) ->
    nextId = client.incr NEXT_ID_KEY, _
    content = new Content nextId, url
    id = content.id
    idKey = getIdKey id
    value = JSON.stringify content
    urlKey = getURLKey url
    client.mset idKey, value,
                urlKey, id,
                _
    content

  @getOrCreate: (url, _) ->
    content = @getByURL url, _
    return content if content?
    @fromURL url, _
