## Development

This app is built on [Haskell].

```bash
git clone https://github.com/zoomhub/zoomhub.git
cd zoomhub
stack setup
```

To start the server, run:

```bash
stack build --file-watch --exec ./scripts/run-development.sh
```

To run the tests, run:

```bash
stack test
```

## Administation

### Prerequisites

-   Install [Ansible](http://docs.ansible.com/).
-   Install [pip](https://pypi.python.org/pypi/pip).
-   Copy `deployment/hosts.sample` to `deployment/hosts` and replace
    `localhost` with your server IP.
-   Copy `credentials/userpassword.sample` to
    `deployment/credentials/userpassword` and paste in your password.

### Commands

`zh` is a script for managing ZoomHub. Currently, we support the following
commands:

-   `./zh bootstrap`: Bootstrap server(s); only required once per user.
-   `./zh ping`: Ping production machines.
-   `./zh setup`: Setup server(s).
-   `./zh deploy`: Deploy latest app code from Git.


[Haskell]: https://www.haskell.org
