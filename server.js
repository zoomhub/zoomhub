var http = require('http');

var PORT = 80;
var IP = '127.0.0.1';

http.createServer(function (req, res) {
  res.writeHead(200, {'Content-Type': 'text/plain'});
  res.end('Hello World\n');
}).listen(PORT, IP);

console.log('Server running at http://' + IP + ':' + PORT + '/');
