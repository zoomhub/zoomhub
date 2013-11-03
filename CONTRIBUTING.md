## Development

This app is built on [Node.js](http://nodejs.org/). Install the latest stable
build (v0.10.x currently), which should install npm, then run:

```bash
npm install
```

To start the server, run:

```bash
npm start
```

To run the tests, run:

```bash
npm test
```

The tests are written for [Mocha](http://visionmedia.github.io/mocha/)'s
[`exports` interface](http://visionmedia.github.io/mocha/#exports-interface).
They use [Chai](http://chaijs.com/) for assertions and
[Supertest](https://github.com/visionmedia/supertest) for API requests.

Portions of this code use [CoffeeScript](http://coffeescript.org/) and/or
[Streamline](https://github.com/Sage/streamlinejs) for now, but we're open to
changing that as this project matures and expands.

To debug via [node-inspector](https://github.com/node-inspector/node-inspector),
run:

```
npm run debugger
```

The CoffeeScript and Streamline code will show up as compiled JS currently;
while source maps get generated internally, they don't get outputted to files
currently (which the debugger needs) when you run the code directly.
Bugs are open to fix that!

To support no-hassle debugging, the server runs in Node's `--debug` mode
always right now. We'll figure out a way to turn that on/off down the road.

To set a breakpoint from code, you can just write the `debugger` keyword.


## Discussion

You can join our [Google Group](https://groups.google.com/group/zoomhub) or
email us at [zoomhub@googlegroups.com](mailto:zoomhub@googlegroups.com).


## Issues

Feel free to file issues in [GitHub Issues](https://github.com/zoomhub/zoomhub/issues).
That's also where we're tracking TODOs and remaining work.

