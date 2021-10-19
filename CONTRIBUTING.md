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

Build and run the backend:

```bash
./zh run
```

Run the frontend:

```bash
cd frontend && npx snowpack dev
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

[haskell]: https://www.haskell.org
[homebrew]: http://brew.sh
[stack]: http://docs.haskellstack.org/en/stable/README.html
