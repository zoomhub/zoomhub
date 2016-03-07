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

-   Install [Ansible]: `brew install ansible`.
    We have tested our setup with Ansible 1.9.4.

### Commands

`./zh` is a script for managing ZoomHub. We support the following commands:

-   `./zh ops bootstrap [admin|production|staging]`:
    Bootstrap admin user on server(s).
    **IMPORTANT:** Can only be run once per server!
-   `./zh ops ping [admin|production|staging]`: Ping servers.
-   `./zh ops setup-server [admin|production|staging]`:
    Basic setup server(s) for all servers.
-   `./zh ops setup-admin-server`: Set up admin server.
-   `./zh ops setup-web-server [production|staging]`: Set up web server(s).


[Ansible]: http://docs.ansible.com
[Haskell]: https://www.haskell.org
[Homebrew]: http://brew.sh
[Stack]: http://docs.haskellstack.org/en/stable/README.html
