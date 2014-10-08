var PipeLines = require('pipe-lines');

/**
 * getIds
 *
 * @description will return a list of ids from a top ids file
 * @param path
 * @param callback
 */
exports.getIds = function (stream, callback) {
  var ids = [];

  var pipe = new PipeLines();

  pipe.on('data', function (data) {
    ids.push(data.toString().split(':')[0]);
  });

  stream.on('error', function (err) {
    callback(err);
  });

  stream.on('end', function () {
    callback(null, ids);
  });

  stream.pipe(pipe);
};