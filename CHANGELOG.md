# ZoomHub

## 0.0.4 - October 1, 2014

-   Upgrade `coffee-script` to 1.8.0.
-   Use more `default` Express logger format for more detailed logs in
    production, incl. timestamps.
-   Switch from [`streamline-express`][npm-streamline-express] to
    [`express-streamline`][npm-express-streamline].
-   Use `npm start` for `./zh run`.
-   Fix Node.js PPA version: `0.10.32-1chl1~precise1`.
-   Fix Redis setup:
    -   Upgrade PPA dependency: `2:2.8.17-1chl1~precise1`
    -   Start Redis before web server.
    -   Fix path to Redis database file: `/var/lib/redis/`.
    -   Fix path to `redis-server` executable: `/usr/bin/redis-server`
    -   Change path to log file: `/var/log/redis/redis-server.log`.

## 0.0.3 - September 22, 2014

-   Add `DZIParser` and `Embed` modules to read created DZIs and create
    OpenSeadragon embed JS, respectively.
-   Change the ZoomHub APIs to match the zoom.it APIs more closely.

## 0.0.2 — November 3, 2013

-   [ZH-22]: Persist metadata using [Redis].
-   Implement basic `Content` model.
-   Extract `Fetcher` and `Processor` from inline code.
-   Link to metadata from view page.
-   Add support for [VIPS] to improve speed of generating DZIs:
    -   Upgrade [deepzoomtools] to version 0.0.4.
    -   Setup [Ansible] [VIPS] role.
-   Improve [Ansible] setup and deployment:
    -   Separate boostrap from setup phase. Bootstrap requires `root` access
        to create admin user so setup can simply use admin user.
    -   Run app on port 3000 and map it to port 80. Allows us to run the app
        with an unprivileged admin user instead of `root`.

## 0.0.1 — October 19, 2013

-   [ZH-18]:  Setup basic deployment on [Rackspace].
-   Add [LICENSE].


[Ansible]: http://www.ansibleworks.com/
[deepzoomtools]: https://github.com/openzoom/node-deepzoomtools
[LICENSE]: LICENSE
[npm-express-streamline]: https://www.npmjs.org/package/express-streamline
[npm-streamline-express]: https://www.npmjs.org/package/streamline-express
[Rackspace]: http://www.rackspace.com/
[Redis]: http://redis.io/
[VIPS]: http://www.vips.ecs.soton.ac.uk/index.php?title=VIPS

[ZH-18]: https://github.com/zoomhub/zoomhub/issues/18
[ZH-22]: https://github.com/zoomhub/zoomhub/issues/22
