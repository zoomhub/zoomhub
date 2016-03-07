# Development

ZoomHub is built on [Haskell]. We use [Stack] to manage dependencies, build, as
well as run the app.

### Prerequisites

- [Homebrew]

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


[Haskell]: https://www.haskell.org
[Homebrew]: http://brew.sh
[Stack]: http://docs.haskellstack.org/en/stable/README.html
