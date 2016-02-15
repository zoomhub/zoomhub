## Development

This app is built on [Haskell].

To set up the project, run:

```bash
git clone https://github.com/zoomhub/zoomhub.git
cd zoomhub
./zh install
```

To start the server, run:

```bash
./zh run
```

To run the tests, run:

```bash
./zh test
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
