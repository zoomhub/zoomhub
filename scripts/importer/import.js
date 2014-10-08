#!/usr/bin/env node

var blobLoader = require('./blob-loader'),
    idLoader = require('./id-loader'),
    pkgcloud = require('pkgcloud'),
    request = require('request'),
    async = require('async'),
    http = require('http'),
    https = require('https'),
    program = require('commander'),
    fs = require('fs');

http.globalAgent.maxSockets = Infinity;
https.globalAgent.maxSockets = Infinity;

program
    .version('0.0.1')
    .option('-u, --username [username]', 'Username')
    .option('-p, --password [password]', 'Password')
    .option('-r, --region [region]', 'Region')
    .option('--useInternal')
    .option('--topFiles [path]')
    .parse(process.argv);

var client = pkgcloud.providers.rackspace.storage.createClient({
  username: program.username,
  password: program.password,
  useInternal: program.useInternal,
  region: program.region
});

var path = '/var/data/zoomhub/content-by-id',
    processed = {};

try {
  processed = require('./processed.json');
}
catch (e) {

}

client.on('log::error', function (message, object) {
  if (object) {
    console.log(this.event.split('::')[1] + ': ' + message);
    console.dir(object);
  }
  else {
    console.log(this.event.split('::')[1] + ': ' + message);
  }
});

console.log('Loading Ids');

idLoader.getIds(program.topFiles, function (err, files) {
  if (err) {
    console.error(err);
    process.exit(1);
  }

  var stats = {
    ids: 0,
    blobs: 0,
    data: 0
  };

  console.log('Loaded ' + files.length + ' IDs');

  setInterval(function () {
    console.dir(stats);
  }, 30000);

  async.forEachLimit(files, 5, function (file, next) {
    stats.ids++;
    console.log('Loading: ' + file);
    if (processed[file]) {
      console.log('Already Processed: ' + file);
      return next();
    }
    blobLoader.getBlobs(file, function (err, response) {
      if (err) {
        console.log('Skipping: ' + file);
        console.error(err);
        return next();
      }

      var destDzi = client.upload({
        container: 'content',
        remote: response.dziPath.replace('/content/', '/dzis/'),
        contentType: 'application/xml'
      });

      destDzi.on('error', function (err) {
        //console.log('Error: ' + blob);
        next(err);
      });

      destDzi.on('success', function (upload) {
        async.forEachLimit(response.blobs, 10, function (blob, blobNext) {
          stats.blobs++;
          var sourceBlob = request(blob);

          var destBlob = client.upload({
            container: 'content',
            remote: sourceBlob.uri.pathname.replace('/content/', '/dzis/')
          });

          sourceBlob.on('response', function (response) {
            stats.data += parseInt(response.headers['content-length']);
            response.headers = {
              'content-type': response.headers['content-type'],
              'content-length': response.headers['content-length']
            };
          });

          destBlob.on('error', function (err) {
            //console.log('Error: ' + blob);
            blobNext(err);
          });

          destBlob.on('success', function (upload) {
            //console.log('Success: ' + blob);
            blobNext();
          });

          sourceBlob.pipe(destBlob);

        }, function (err) {
          if (err) {
            next(err);
            return;
          }

          processed[file] = true;
          var update = fs.createWriteStream('./processed.json');

          update.end(JSON.stringify(processed), function () {
            next(err);
          });
        });
      });

      destDzi.end(response.dzi);

    });
  }, function (err) {
    if (err) {
      console.error(err);
    }
    console.dir(stats);
  });
});

