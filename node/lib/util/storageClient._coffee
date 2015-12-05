config = require '../../config'
pkgcloud = require 'pkgcloud'


if not config.RACKSPACE_USERNAME
    throw new Error "`RACKSPACE_USERNAME` configuration variable must be set;
        got: #{config.RACKSPACE_USERNAME}"

if not config.RACKSPACE_API_KEY
    throw new Error "`RACKSPACE_API_KEY` configuration variable must be set;
        got: #{config.RACKSPACE_API_KEY}"

module.exports = storageClient = pkgcloud.storage.createClient
    provider: 'rackspace'
    username: config.RACKSPACE_USERNAME
    apiKey: config.RACKSPACE_API_KEY
    region: config.CONTENT_REGION
