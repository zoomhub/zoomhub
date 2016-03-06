# ZoomHub

[![Build Status](https://circleci.com/gh/gasi/zoomhub.svg?style=shield&circle-token=4e2b7e7de6ab3177d1e744d04dbacccb8c6cd68b)](https://circleci.com/gh/gasi/zoomhub)
[![Gitter](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/zoomhub/zoomhub?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

An open-source cloud service for viewing arbitrarily high-resolution zoomable
images.

Inspired by, and has taken over, the similar former Microsoft service
**[zoom.it]**.
This is a full, from-scratch rewrite, brought to you by the same developers who
built the original **[zoom.it]**.


## Usage

This service is running live at **[zoom.it]**.
We aren’t accepting any new content for now, but all old **[zoom.it]** content
should continue to work.

This service also includes a REST API, available at **[api.zoom.it][api.zoom.it]**
(e.g. [`/v1/content/4rcn`](http://api.zoom.it/v1/content/4rcn)).
For details and API documentation, see **[API.md](./API.md)**.

You can also run this code and host the service on your own.
The codebase is still a work-in-progress, but we can try to help if you need.

If you wish to run this on your own, or to contribute to our development,
please see **[CONTRIBUTING.md](./CONTRIBUTING.md)** for instructions.


## Discussion

You can join our [Google Group](https://groups.google.com/group/zoomhub)
or email us at [zoomhub@googlegroups.com](mailto:zoomhub@googlegroups.com).


## Issues

Feel free to file bugs, request features, and ask questions on
[GitHub Issues](https://github.com/zoomhub/zoomhub/issues).
That’s also where we’re tracking TODOs and remaining work.


## Credits

Special thanks to:

- **Bill Crow** and **David Vos** for keeping [zoom.it] alive at Microsoft
  through the years, and helping us transition it when its time came.
  They moved mountains to get us both the data and the domain.

- **[Rackspace](http://www.rackspace.com/)** for hosting and running this
  service free of charge. Rackspace ♥︎ open-source, and open-source ♥︎ Rackspace.

- **[OpenSeadragon](http://openseadragon.github.io/)** for providing the
  zooming viewer.

- **[VIPS](http://www.vips.ecs.soton.ac.uk/index.php?title=VIPS)** for
  providing blazing-fast DZI generation.

- All of our
  **[contributors](https://github.com/zoomhub/zoomhub/graphs/contributors)** for
  their time and energy.
  This project is entirely a labor of love, and all of us have our own day jobs.
  Maintaining any open-source project is work, but running a live, production
  service on top of that is something even more.

- [Marcin Mierzejewski](https://github.com/zenzire) for the Ansible Ubuntu
  setup scripts: https://github.com/zenzire/ansible-bootstrap-ubuntu

## License

This code is open-source under the [MIT license](./LICENSE).


[zoom.it]: http://zoom.it/
[api.zoom.it]: http://api.zoom.it/
