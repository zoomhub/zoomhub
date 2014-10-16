# ZoomHub

[![Build Status](https://travis-ci.org/zoomhub/zoomhub.svg?branch=master)](https://travis-ci.org/zoomhub/zoomhub)
[![Gitter](https://badges.gitter.im/Join Chat.svg)](https://gitter.im/zoomhub/zoomhub?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

This is the beginning of an open-source codebase for a cloud zooming service,
like [Zoom.it].


## Contributing

See [CONTRIBUTING.md] for details.


## Setup

-   [Install Node.js][node-installation].
-   [Install Redis][redis].
-   Install dependencies: `./zh install`


## Optional Prerequisites

-   [Ansible][ansible-installation] for deployment.
-   [pip] for installing `passlib` (see below).
-   Python `passlib` for generating passwords: `pip install passlib`.


## API

See: [API »](./API.md)


## Embedding with OpenSeadragon

As it currently stands, the embed code needs to read an existing DZI. From the
information contained in the DZI, it instantiates an OpenSeadragon viewer and
attaches that to its own element in the DOM of the webpage that the embed is placed in.


## Administation

### Prerequisites

-   Copy `deployment/hosts.sample` to `deployment/hosts` and replace
    `localhost` with your server IP.
-   Copy `credentials/userpassword.sample` to
    `deployment/credentials/userpassword` and paste in your password.

### Commands

`zh` is a script for managing ZoomHub. Currently, we support the following
commands:

-   `./zh install`: Install dependencies.
-   `./zh run`: Run app locally.
-   `./zh bootstrap`: Bootstrap server(s); only required once per user.
-   `./zh ping`: Ping production machines.
-   `./zh setup`: Setup server(s).
-   `./zh deploy`: Deploy latest app code from Git.


## License

The MIT License. See [LICENSE][] file.



[ansible-installation]: http://www.ansibleworks.com/docs/intro_installation.html
[CONTRIBUTING.md]: CONTRIBUTING.md
[LICENSE]: LICENSE
[node-installation]: http://nodejs.org/download/
[pip]: https://pypi.python.org/pypi/pip
[redis]: http://redis.io/
[Zoom.it]: http://zoom.it/
