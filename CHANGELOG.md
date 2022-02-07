# ZoomHub

## 2021-02-07-1

- Set canonical URLs to direct traffic from zoom.it to zoomhub.net.
- `processContent`:
  - Clean up `/tmp` on every invocation to free up disk space.
  - Report disk space.

## 2021-11-19-1

- Improve landing page
  - Make demo mode explicit to allow for additional content below
  - Add **Embed** section
  - Add **API** section
- Set Safari toolbar color

## 2021-11-11-1

- Show progress bar for uploads.
- Add GIF (`image/gif`) to list of allowed file uploads.

## 2021-11-10-2

- Restrict uploads to JPEG, PNG, and TIFF files.

## 2021-11-10-1

- Restore upload UI. We have observed many invalid image URL submissions as many
  point to non-image resources that we currently don’t support, e.g. images in
  HTML image galleries, document HTML pages, YouTube, etc.
- Improve colors of text inputs in dark theme.

## 2021-10-21-1

- Make homepage more mobile friendly.
- Run full development stack via `./zh run`: ngrok, Haskell web API, and
  Snowpack frontend development server.
- Dynamically set API base URL.
- Reduce logging in background worker to increase signal to noise ratio.
- Call AWS Lambda function alias based on `ZH_ENV` environment variable.

## 2021-10-20-1

- Accept submissions from image URLs instead of uploads only.
- Temporarily remove upload UI until it’s ported to new frontend.
- Add frontend project using:
  - [React.js] for interactive components
  - [Tailwind CSS] for styling
  - [Snowpack] for development server and production builds

## 2021-09-30-1

- Improve homepage copy.

## 2021-09-29-2

- Improve homepage design.
- Swap cover image for a panorama.

## 2021-09-29-1

- Redirect on verification link if content is completed.
- Improve copy of verification email.
- Homepage: Keep upload card in view (via scrolling) when changing states.
- Use HTTPS for static and cache content URLs by default.
- Use `neat-interpolation` for cleaner multiline strings with interpolated
  values.
- `zh`: Fix slow compilation due to optimization flag change by adding `--fast`
  to match all the other `stack build` calls.

## 2021-09-28

- Improve performance of `getNextUnprocessed` query using indexes on `content`.
- When resetting content, delete any associated image.
- Temporarily reduce max upload size from 100MB to 50MB due to AWS Lambda
  scratch disk limitations (500MB maximum).
- Improve logging for when we try to notify someone about content without a
  submitter email and/or verification token.
- Remove unused `TempPath` since we stopped doing image processing on web
  server.

## 2021-09-26

- [#172][zh-22] Auto-rotate images based on EXIF data.
- Worker: Increase poll interval from 3 to 5 seconds.
- Add `PUT /v1/content/:id/reset` for admins to reset content, e.g. after it
  failed or we improved the processing pipeline. An example is for images that
  were incorrectly rotated but can be reprocessed after fixing that issue
  [#172][zh-22].
- Ensure running and testing the app doesn’t cause a full rebuild by matching
  `stack` (GHC) flags.

## 2021-09-25

- 🎉 MVP
  - Allow uploads via homepage
  - Send email verification
  - Verification landing page
  - Process image via AWS Lambda
  - Redirect upon success

## 2021-09-24

- Send submitter an email with a verification link to authenticate the upload.
  After verification, our background worker picks up unprocessed submissions and
  processes them via AWS Lambda.
- Introduce `ZH_ENV = "development" | "test" | "production"` environment
  variable for controlling certain actions, e.g. not sending emails during
  testing.
- Set content version to `5` on new submissions. These are submissions that
  have a submitter email and verification token.
- Control logging via `LOG_LEVEL` environment variable. Extract `LogLevel`
  module and introduce `logLevel` configuration for controlling what level of
  logs we want to capture.
- Pipe AWS logs through our own JSON logger.
- Reduce log level of many worker operations to reduce noise.
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
[react.js]: https://reactjs.org/
[redis]: http://redis.io/
[sqlite]: https://www.sqlite.org/index.html
[squeal]: https://hackage.haskell.org/package/squeal-postgresql
[tailwind css]: https://tailwindcss.com
[snowpack]: https://www.snowpack.dev
[vips]: http://www.vips.ecs.soton.ac.uk/index.php?title=VIPS
[zh-172]: https://github.com/zoomhub/zoomhub/issues/#172
[zh-18]: https://github.com/zoomhub/zoomhub/issues/18
[zh-22]: https://github.com/zoomhub/zoomhub/issues/22
