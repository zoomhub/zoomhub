var redis = require('redis'),
    pkgcloud = require('pkgcloud'),
    cloudApp = require('./cloud-app'),
    idLoader = require('./id-loader');

var programOpts = cloudApp.loadProgram(process.argv);

var redisClient = redis.createClient(),
    storageClient = pkgcloud.providers.rackspace.storage.createClient(programOpts.cloudOptions);

idLoader.getIds(storageClient.download({
  container: 'archive',
  remote: 'total.txt'
}), function(err, ids) {
  console.log(ids.length);
  redisClient.sadd('ids-to-import', ids, function(err) {
    if (err) {
      console.error(err);
    }
    process.exit(err ? 1 : 0);
  });
});

