## Development

This app is built on [Haskell]. We use [Stack] to manage dependencies, build, as
well as run the app.

#### Prerequisites

-   [Homebrew]

### Setup

Set up the app:

```bash
git clone https://github.com/zoomhub/zoomhub.git
cd zoomhub
./zh init
```

### Run

Build and run the app:

```bash
./zh run
```

### Test

Run the app tests:

```bash
./zh test
```

### Lint

Run lint tools. Note: `stylish-haskell` can perform in-place fixes, so we
enforce that all changes are committed to Git:

```bash
./zh lint
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
[Homebrew]: http://brew.sh/
[Stack]: http://docs.haskellstack.org/en/stable/README.html
