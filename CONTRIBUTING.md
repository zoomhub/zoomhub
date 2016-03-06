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

-   Install [Ansible](http://docs.ansible.com/): `brew install ansible`.
    We have tested our setup with Ansible 1.9.4.

### Commands

`./zh` is a script for managing ZoomHub. Currently, we support the following
commands:

-   `./zh ops create-admin-user [admin|production|staging]`:
    Bootstrap admin user on server(s); only required once per server.
-   `./zh ops ping [admin|production|staging]`: Ping production machines.
-   `./zh ops setup [admin|production|staging]`: Setup server(s).


[Haskell]: https://www.haskell.org
[Homebrew]: http://brew.sh/
[Stack]: http://docs.haskellstack.org/en/stable/README.html
