fs = require 'fs'
path = require 'path'
request = require 'request'


# Public API
module.exports = class Fetcher
  constructor: (@path) ->

  fetch: (content, callback) ->
    source = content.urls.source
    destination = path.join @path, content.id.toString()

    writer = request(source).pipe fs.createWriteStream destination
    writer.on 'readable', ->
      console.log 'readable'
    writer.on 'data', (chunk) ->
      console.log 'data', chunk
    writer.on 'error', (error) ->
      callback error
    writer.on 'finish', (error) ->
      if error?
        return callback error
      callback null, destination
