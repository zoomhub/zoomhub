#
# This file lists the runtime configs of this app, and provides example
# values to enable local development.
#
# Usage:
#
#   config = require './config'
#   ...
#   app.listen config.PORT
#
# Important: don't place any production values or sensitive secrets here!
# Instead, set those values as environment variables on the running server.
#


## HELPERS:

get = (name, defaultValue) ->
    process.env[name] or defaultValue

getInt = (name, defaultValue) ->
    str = get name
    int = parseInt str, 10
    if isNaN int then defaultValue else int


## PUBLIC:

@PORT = getInt 'PORT', 8000

