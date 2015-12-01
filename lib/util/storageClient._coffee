config = require '../../config'
pkgcloud = require 'pkgcloud'


module.exports = storageClient = pkgcloud.storage.createClient
    provider: 'rackspace'
    username: config.RACKSPACE_USERNAME
    apiKey: config.RACKSPACE_API_KEY
    region: config.CONTENT_REGION
