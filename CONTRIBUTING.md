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

Lint code:

```bash
./zh lint
```

### Format

Format code:

```bash
./zh format
```


[Haskell]: https://www.haskell.org
[Homebrew]: http://brew.sh
[Stack]: http://docs.haskellstack.org/en/stable/README.html
