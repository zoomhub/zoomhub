var fs = require('fs'),
    PipeLines = require('pipe-lines');

/**
 * getIds
 *
 * @description will return a list of ids from a top ids file
 * @param path
 * @param callback
 */
exports.getIds = function (path, callback) {
  var ids = [];

  var pipe = new PipeLines();

  pipe.on('data', function (data) {
    ids.push(data.toString().split(':')[0]);
  });

  var stream = fs.createReadStream(path);

  stream.on('error', function (err) {
    callback(err);
  });

  stream.on('end', function () {
    callback(null, ids);
  });

  stream.pipe(pipe);
};