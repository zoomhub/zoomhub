var async = require('async'),
    cloudApp = require('./cloud-app'),
    dziLoader = require('./dzi-loader'),
    pkgcloud = require('pkgcloud'),
    prettyBytes = require('pretty-bytes'),
    redis = require('redis'),
    request = require('request');

var programOpts = cloudApp.loadProgram(process.argv, [ '--id [value]' ]);

var metrics = {
  id: programOpts.program.id,
  blobs: 1, // we always upload a dzi
  size: 0,
  start: new Date().getTime()
};

process.send({ type: 'log', level: 'verbose', message: 'starting import', payload: metrics });

var redisClient = redis.createClient(),
    storageClient = pkgcloud.providers.rackspace.storage.createClient(programOpts.cloudOptions);

// make sure we log emitted errors
storageClient.on('log::*', function (message, object) {
  process.send({
    type: 'log',
    level: this.event.split('::')[1],
    message: message,
    payload: object
  });
});

dziLoader.loadDzi(metrics.id, function (err, response) {
  if (err) {
    process.send({
      type: 'error',
      message: 'Unable to load the DZI from cache.zoom.it',
      payload: err
    });
    process.exit(1);
  }

  // account for the dzi size
  metrics.size += response.dzi.length;
  process.send({ type: 'metrics::size', size: response.dzi.length });

  var destDzi = storageClient.upload({
    container: 'content',
    remote: response.dziPath.replace('/content/', '/dzis/'),
    contentType: 'application/xml'
  });

  destDzi.on('error', function (err) {
    process.send({
      type: 'error',
      message: 'Error uploading stream for DZI',
      payload: err
    });
    process.exit(1);
  });

  destDzi.on('success', function () {

    // now lets upload the dzis
    async.forEachLimit(response.blobs, 10, function (blob, next) {
      metrics.blobs++;
      process.send({ type: 'metrics::blob' });
      process.send({ type: 'log', level: 'verbose', message: 'uploading blob', payload: blob });

      var sourceBlob = request(blob);

      var destBlob = storageClient.upload({
        container: 'content',
        remote: sourceBlob.uri.pathname.replace('/content/', '/dzis/')
      });

      sourceBlob.on('response', function (response) {
        metrics.size += parseInt(response.headers['content-length']);
        process.send({ type: 'metrics::size', size: parseInt(response.headers['content-length']) });
        response.headers = {
          'content-type': response.headers['content-type'],
          'content-length': response.headers['content-length']
        };
      });

      destBlob.on('error', function (err) {
        process.send({
          type: 'error',
          message: 'Error uploading stream for blob: ' + blob,
          payload: err
        });
      });

      destBlob.on('success', function () {
        next();
      });

      sourceBlob.pipe(destBlob);

    }, function (err) {
      if (err) {
        process.exit(err ? 1 : 0);
      }

      metrics.end = new Date().getTime() - metrics.start;

      redisClient.sadd('ids-processed', JSON.stringify({
        id: metrics.id,
        blobs: metrics.blobs,
        files: metrics.files,
        size: prettyBytes(metrics.size),
        time: metrics.end / 1000 + 's',
        start: metrics.start
      }), function(err) {
        if (err) {
          process.send({
            type: 'error',
            message: 'Error saving metrics for DZI',
            payload: err
          });
          process.send({
            type: 'error',
            message: 'Error saving metrics for DZI',
            payload: metrics
          });
        }

        process.exit(err ? 1 : 0);
      });
    });
  });

  destDzi.end(response.dzi);

});
