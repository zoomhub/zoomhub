#!/usr/bin/env node

var async = require('async'),
    cloudApp = require('./cloud-app'),
    fork = require('child_process').fork,
    logging = require('./logging'),
    prettyBytes = require('pretty-bytes'),
    redis = require('redis');

var programOpts = cloudApp.loadProgram(process.argv),
    redisClient = redis.createClient(),
    log = logging.getLogger(process.env.IMPORT_LOG_LEVEL || 'debug'),
    workers = [];

for (var i = 0; i < require('os').cpus().length; i++) {
  workers.push(i);
}

var metrics = {
  blobs: 0,
  files: 0,
  size: 0,
  start: new Date().getTime()
};

log.info('Starting ' + require('os').cpus().length + ' processes');

var interval = setInterval(function () {
  log.info({
    blobs: metrics.blobs,
    files: metrics.files,
    size: prettyBytes(metrics.size),
    time: (new Date().getTime() - metrics.start) / 1000 + 's'
  });
}, 30000);

async.forEachLimit(workers, workers.length, function(worker, next) {

  var workerMetrics = {
    blobs: 0,
    files: 0,
    size: 0,
    start: new Date().getTime()
  };

  function importImage(callback) {
    redisClient.spop('ids-to-import', function(err, id) {
      if (err) {
        callback(err);
        return;
      }

      if (!id) {
        workerMetrics.end = new Date().getTime() - workerMetrics.start;
        log.info('Process ' + worker + ' finished', {
          blobs: workerMetrics.blobs,
          files: workerMetrics.files,
          size: prettyBytes(workerMetrics.size),
          time: workerMetrics.end / 1000 + 's'
        });

        callback();
        return;
      }

      var args = ['--id', id,
        '-u', programOpts.cloudOptions.username,
        '-p', programOpts.cloudOptions.password,
        '-r', programOpts.cloudOptions.region];

      if (programOpts.cloudOptions.useInternal) {
        args.push('--useInternal');
      }

      // spawn the process
      var childProcess = fork('scripts/importer/import-id.js', args);

      childProcess.on('close', function(code) {
        if (code != 0) {
          // first, re-add the id to the queue
          redisClient.sadd('ids-to-import', id, function (err2) {
            if (err2) {
              log.error('Unable to requeue "' + id + '"', err2);
            }

            log.error('Error processing "' + id + '"'. err);
            callback(err);
          });

          return;
        }

        metrics.files++;
        workerMetrics.files++;

        importImage(callback);
      });

      childProcess.on('message', function(data) {
        switch (data.type) {
          case 'error':
            log.error(data.message, data.payload);
            break;
          case 'log':
            log[data.level](data.message, data.payload);
            break;
          case 'metrics::size':
            metrics.size += data.size;
            workerMetrics.size += data.size;
            break;
          case 'metrics::blob':
            metrics.blobs++;
            workerMetrics.blobs++;
            break;
        }
      });
    });
  }

  importImage(next);

}, function(err) {
  if (err) {
    log.error(err);
  }

  metrics.end = new Date().getTime() - metrics.start;
  log.info('Completed processing jobs in queue', {
    blobs: metrics.blobs,
    files: metrics.files,
    size: prettyBytes(metrics.size),
    time: metrics.end / 1000 + 's'
  });
  process.exit(err ? 1 : 0);
});
