fs = require 'fs'
path = require 'path'
request = require 'request'


# Public API
module.exports = class Fetcher
  constructor: (@path) ->

  fetch: (content, callback) ->
    source = content.url
    destination = path.join @path, content.id.toString()

    reader = request source
    reader.on 'error', (error) ->
      callback error

    writer = fs.createWriteStream destination
    writer.on 'error', (error) ->
      callback error
    writer.on 'finish', (error) ->
      callback error, destination

    reader.pipe writer
