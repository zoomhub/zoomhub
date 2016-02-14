#!/usr/bin/env node

// this will look through the total.txt, and update any JSON (with no errors)
// to progress: 1 and ready: true
//
// node mark-complete.js /path/to/total.txt /path/to/content-by-id
//
//

var async = require('async'),
  fs = require('fs');

var file = process.argv[2],
  path = process.argv[3];

fs.readFile(file, function(err, data) {
  var ids = data.toString().split('\n');

  ids = ids.map(function(id) {
    return id ? id.split(':')[0] : null;
  });

  var missing = 0;

  async.forEachLimit(ids, 10, function(id, next) {

    if (!id) {
      next();
      return;
    }

    var normalizedId = id.replace(/([A-Z])/g, '_$1'),
        fileName = path + '/' + normalizedId + '.json';
    fs.readFile(fileName, function(err, json) {
      // if missing, log and skip
      if (err) {
        console.log('Skipping missing file: ' + fileName);
        missing++;
        next();
        return;
      }

      // if JSON won't parse, log and skip
      try {
        json = JSON.parse(json.toString());
      }
      catch (e) {
        console.log('Skipping malformed json file: ' + fileName);
        next();
        return;
      }

      // confirm we're normalized
      if (!json.dzi) {
        console.log('Skipping missing dzi file: ' + fileName);
        next();
        return;
      }

      json.ready = true;
      json.progress = 1;

      var writable = fs.createWriteStream(fileName);

      writable.end(JSON.stringify(json), next);
    });
  }, function(err) {
    if (err) {
      console.dir(err);
      process.exit(1);
      return;
    }

    console.log('total missing: ' + missing);

    process.exit(0);
  });
});