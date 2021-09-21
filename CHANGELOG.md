# ZoomHub

## Unreleased

- Remove unused `S3_CACHE_BUCKET` from Haskell web server.

## 2021-09-09

- Generate verification token for each new submission. This will be used to
  send an email verification link to the submitter.

## 2021-02-28

- Require email for each submission:
  - `/v1/content/upload?email=<email>`
  - `/v1/content?email=<email>&url=<url>`

## 3.1.0-alpha.1 – January 18, 2021

- Add AWS Lambda worker for converting content into Deep Zoom Images (DZIs)
  using VIPS:
  - Package and deploy code from CI.
  - Add `API_USERNAME` and `API_PASSWORD` for authenticating Lambda worker.
  - Add authenticated endpoint `PUT /v1/content/:id/completion` for reporting
    AWS Lambda worker processing completions.
  - Remove unused `ZoomHub.Pipeline` module.
- Introduce distinction between `JPG` and `JPEG` tile formats. By default, VIPS
  outputs DZI tiles with `.jpeg` extension. To accommodate that, we needed
  distinguish tiles by file extension. Ultimately, we worked around it by
  renaming tiles before upload to S3.
- Add `amazonka` dependency for invoking AWS Lambda worker.
- **Development:** Normalize GHC flags to avoid multiple recompilations.
- **Development:** Fix database setup by separating content insertions and
  adjustment of PostgreSQL sequences.

## 3.0.0 – December 12, 2020

- Enable HTTPS.

## 3.0.0-rc.2 – November 12, 2020

- **Uploads**
  - Add `/v1/content/upload` endpoint for AWS S3 presigned POST using MinIO.
  - Add `UPLOADS=true|false` environment variable for enabling/disabling
    uploads.
  - Add `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `S3_CACHE_BUCKET`, and
    `S3_SOURCES_BUCKET` environment variables for AWS/MinIO configuration.
- Add `CONTENT_BASE_URI` environment variable to replace Rackspace container
  based URL resolution.
- Limit logging of request body to 512 bytes.

## 3.0.0-rc.1 – July 7, 2020

- Serving all production traffic.
- Auto-formatted Haskell code using [Ormolu].

## 3.0.0-alpha.2 – June 28, 2020

- Restore `/version` endpoint.

## 3.0.0-alpha.1 – June 28, 2020

- Migrate hosting from Rackspace to Amazon AWS:
  - Rackspace Cloud Servers → AWS Elastic Beanstalk (EC2 + ECS + ECR)
  - Rackspace Cloud Files → AWS S3
  - Keter bundle → Docker container
  - Local SQLite file → AWS RDS PostgreSQL
  - Splunk → TBD
- Remove archived files.
- Remove old SQLite development database.
- Remove Ansible based ops setup.
- Remove Rackspace Cloud Files to AWS S3s migration scripts.
- Format Markdown + HTML using Prettier.
- Add `stack.yaml.lock` file.

## 3.0.0-alpha – June 27, 2020

- To enable concurrent submissions, migrate storage engine from [SQLite],
  which only allows a single write operation due to a file lock, to
  [PostgreSQL] using [Squeal], a type-safe embedding of PostgreSQL in Haskell.
  - Perform type-safe database migrations using `migrate-database` helper.
  - Set up PostgreSQL trigger for generating new content hash IDs.
  - Add full test suite for ZoomHub API.
  - **Ops**
    - Add infrastructure for setting up RDS PostgreSQL instance.
    - Add database migration scripts.
- Combine `PROCESS_EXISTING_CONTENT` and `PROCESS_NEW_CONTENT` environment
  variables into
  `PROCESS_CONTENT='ProcessNoContent|ProcessExistingContent|ProcessExistingAndNewContent'`.
  This was done because `PROCESS_EXISTING_CONTENT=0` and
  `PROCESS_NEW_CONTENT=1` is an invalid state. We do not spin up any workers
  unless `PROCESS_CONTENT=ProcessExistingContent|ProcessExistingAndNewContent`.
- Upgrade Stack LTS from 12.6 to 14.14.

## 2.0.0 – April 14, 2016

- Add infrastructure for background processing of existing content.
- Add `PROCESSING_WORKERS` environment variable for controlling the number
  of workers (green threads) that process existing content.
- Track `numViews` using a sampling rate /ht [@aseemk].
- Order keys of log lines so `time` comes first which helps Splunk
  process them.
- Pretty print certain `HttpException`, most notably `StatusCodeException`
  which previously generated very long log lines due to artificial
  `X-Response-Body-Start` header that included entire error response body.
- Simplify parsing configuration from environment variables.
- Make error handling in `Worker` more robust.
- Track `worker` metadata in logs.

## 0.1.0 – April 10, 2016

- Import original Zoom.it data—`ContentInfo`, `ImageInfo`,
  and `FlickrPhotoInfo`—into new `zoomhub.sqlite3` database.
- Fix incorrect MIME types in original data set.
- Start tracking number of views for a given content using `numViews` column.
- Use new database connection for each request in an unsuccessful attempt to
  mitigate multi-threading issues with SQLite 3.
  See: <https://github.com/IreneKnapp/direct-sqlite/issues/61>
- Add background worker for processing existing, unprocessed content. Picks
  most popular content based on `numViews` data.
- Introduce `ContentType` data type.
- Support existing `zoomit://thumbnail/?url=` URLs as `ContentURI`.
- Add `PROCESS_EXISTING_CONTENT` and `PROCESS_NEW_CONTENT`
  environment variables.
- Log `Config` at startup.
- Introduce `RACKSPACE_CONTAINER` and `RACKSPACE_CONTAINER_PATH` environment
  variables to run existing content (container: `content`, path: `dzis`) and
  new one (container: `cache`, path: `content`; backwards compatibility with
  Zoom.it).
- Improve logging throughout the app using `Logger` and `logT` which lets
  you log duration of operations using `timeItT`.
- Add `type: "access"` field to request log lines to distinguish from
  `type: "app"` application logs.
- Adopt type-safe time units using `time-units` library.
- Switch `duration` request log field into type-safe milliseconds.
- Add global Warp web server exception handler using
  `message: "Web server exception"`.
- Introduce `TEMP_PATH` to replace `DATA_PATH`. The application writes
  temporary data into `$TEMP_PATH/temp`.
- Switch static content hosting to `static.zoomhub.net`.
- Improve VIPS error reporting using `readProcessWithExitCode`.
- Implement DZI XML parsing using `Text.XML.Light` and add tests.
- Implement DZI tile and manifest upload to CloudFiles using `putContent`.
- Add retry logic to database writes and CloudFiles uploads
  using `retry` library.
- Make `contentMIME` type-safe.
- Make `initializedAt` non-null.
- Optimize output binary using GHC `-O3` flag.
- Add basic support for multithreading, e.g. `getNumCapabilities`, etc., but
  disable it until SQLite3 multi-threading issues have been resolved or we
  adopt a different database.

## 0.0.5 – February 14, 2016

- Initial port to Haskell.
- Add basic continuous deployment using [CircleCI].

## 0.0.4 - October 1, 2014

- Upgrade `coffee-script` to 1.8.0.
- Use more `default` Express logger format for more detailed logs in
  production, incl. timestamps.
- Switch from [`streamline-express`][npm-streamline-express] to
  [`express-streamline`][npm-express-streamline].
- Use `npm start` for `./zh run`.
- Fix Node.js PPA version: `0.10.32-1chl1~precise1`.
- Fix Redis setup:
  - Upgrade PPA dependency: `2:2.8.17-1chl1~precise1`
  - Start Redis before web server.
  - Fix path to Redis database file: `/var/lib/redis/`.
  - Fix path to `redis-server` executable: `/usr/bin/redis-server`
  - Change path to log file: `/var/log/redis/redis-server.log`.

## 0.0.3 - September 22, 2014

- Add `DZIParser` and `Embed` modules to read created DZIs and create
  OpenSeadragon embed JS, respectively.
- Change the ZoomHub APIs to match the zoom.it APIs more closely.

## 0.0.2 — November 3, 2013

- [ZH-22]: Persist metadata using [Redis].
- Implement basic `Content` model.
- Extract `Fetcher` and `Processor` from inline code.
- Link to metadata from view page.
- Add support for [VIPS] to improve speed of generating DZIs:
  - Upgrade [deepzoomtools] to version 0.0.4.
  - Setup [Ansible][vips] role.
- Improve [Ansible] setup and deployment:
  - Separate boostrap from setup phase. Bootstrap requires `root` access
    to create admin user so setup can simply use admin user.
  - Run app on port 3000 and map it to port 80. Allows us to run the app
    with an unprivileged admin user instead of `root`.

## 0.0.1 — October 19, 2013

- [ZH-18]: Setup basic deployment on [Rackspace].
- Add [LICENSE].

[@aseemk]: https://github.com/aseemk
[ansible]: http://www.ansibleworks.com/
[circleci]: https://circleci.com
[deepzoomtools]: https://github.com/openzoom/node-deepzoomtools
[license]: LICENSE
[npm-express-streamline]: https://www.npmjs.org/package/express-streamline
[npm-streamline-express]: https://www.npmjs.org/package/streamline-express
[ormolu]: https://hackage.haskell.org/package/ormolu
[postgresql]: https://www.postgresql.org/
[rackspace]: http://www.rackspace.com/
[redis]: http://redis.io/
[sqlite]: https://www.sqlite.org/index.html
[squeal]: https://hackage.haskell.org/package/squeal-postgresql
[vips]: http://www.vips.ecs.soton.ac.uk/index.php?title=VIPS
[zh-18]: https://github.com/zoomhub/zoomhub/issues/18
[zh-22]: https://github.com/zoomhub/zoomhub/issues/22
