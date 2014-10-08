#!/usr/bin/env node

var async = require('async'),
    fs = require('fs'),
    pkgcloud = require('pkgcloud'),
    program = require('commander');

program
    .version('0.0.1')
    .option('-u, --username', 'Username')
    .option('-p, --password', 'Password')
    .option('-r, --region', 'Region')
    .parse(process.argv);

var client = pkgcloud.providers.rackspace.storage.createClient({
  username: program.username,
  password: program.password,
  region: program.region
});

async.forEach(['zoomhub1', 'zoomhub2'], function(file, next) {
  console.log('Downloading ' + file);
  var sourceStream = client.download({
    container: 'archive',
    remote: file + '.zip'
  });

  var destStream = fs.createWriteStream('/var/data/' + file + '.zip');

  destStream.on('end', function() {
    next();
  });

  sourceStream.pipe(destStream);

}, function() {
  console.log('Databases downloaded');
});
