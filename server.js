var http = require('http');

var PORT = 80;

http.createServer(function (req, res) {
  res.writeHead(200, {'Content-Type': 'text/plain'});
  res.end('Hello World\n');
}).listen(PORT);

console.log('ZoomHub running at http://localhost:' + PORT + '/');
