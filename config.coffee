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
path = require 'path'
PORT = 8000

configs = module.exports =

    #
    # What port the app should run on.
    #
    PORT: PORT

    #
    # Base URL of web app
    #
    BASE_URL: "http://localhost:#{PORT}"

    #
    # Base URL of CDN content location
    #
    CDN_URL: "http://content.zoomhub.net"

    #
    # What type of data store to use. Options: 'files', 'redis'.
    # TODO: This probably needs expansion. E.g. if Redis, what URL?
    # (Currently defaults to localhost:6379, with no password.)
    #
    DATA_STORE: 'files'

    #
    # The directory containing our data files.
    #
    DATA_DIR: "#{__dirname}/data"

    #
    # URL path for static assets
    #
    STATIC_DIR: "/static"

    #
    # File system path for static assets
    #
    STATIC_PATH: path.join __dirname, 'public'

    #
    # Subdirectory for DZIs under the static asset tree
    #
    DZI_DIR: "/dzi"

    #
    # Subdirectory for DZIs in the CDN location
    #
    CDN_DZI_DIR: "/dzis"

    #
    # Whether to allow new content or not.
    # Set to false when under load or having issues.
    #
    # NOTE: Set to false right now since our new ID generation is still via
    # Redis, which doesn't know about our flat file data store.
    #
    ALLOW_NEW_CONTENT: false

    #
    # What format we should use with Express logger.
    # The choices are 'dev', 'tiny', 'short', and 'default' (full).
    # Details: http://www.senchalabs.org/connect/logger.html
    #
    EXPRESS_LOGGER_FORMAT: 'dev'


# now go through each config and check if we have any env var overrides.
# env vars are always strings, so use the default values to determine if
# configs should be parsed to numbers or bools.
# (TODO update this loop to support nested configs eventually too.)

for key, defaultValue of configs
    if key of process.env
        val = process.env[key]

        switch typeof defaultValue
            when 'number'
                val = parseInt val, 10
            when 'boolean'
                val = val.toLowerCase() not in [
                    '1', 'true', 'on', 'yes', 'enabled']

        configs[key] = val
