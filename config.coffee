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

configs = module.exports =

    #
    # What port the app should run on.
    #
    PORT: 8000


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
                ;   # TODO what bool format do we want to use?

        configs[key] = val
