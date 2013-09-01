
## Issues

Feel free to file issues on this GitHub repo's issue tracker.


## Discussion

We should eventually have a mailing list. We don't yet.


## Development

This app is built on [Node.js](http://nodejs.org/). Install the latest stable
build (v0.10.x currently), which should install npm, then run:

```
npm install
npm start
```

The code uses [CoffeeScript](http://coffeescript.org/) and [Streamline](
https://github.com/Sage/streamlinejs) for convenience and productivity.


## TODO

### Process/workflow

- Get set up to deploy to various PaaS'es. (Heroku, Rackspace, others?)

- Wire up a config setup, e.g. environment variables overriding a config file.

- Wire up a test runner, e.g. Mocha.

- Wire up Travis eventually for CI once we have tests in place.

### Data layer

- Design a simple data model around image conversions.

- Implement a basic version using in-memory data structures.

- Hook up to a real database eventually. Which one?

### Image tiling

- Implement an image tiler based on e.g. node-canvas?

- Design a simple queue+worker system to do the tiling? Which queue?

### Image storage

- Design a simple adapter interface for file storage implementations.

- Implement a simple adapter for the local file system.

- Eventually implement adapters for Amazon S3, Rackspace Cloud Files, etc.

### API

- Design a simple web API for requesting and fetching conversions.
