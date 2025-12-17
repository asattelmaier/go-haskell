# Go Haskell

## Prerequisites

The project requires the following tools. Using `ghcup` to manage GHC and Cabal is recommended.

* ghc    == 9.6.4
* cabal  >= 3.10
* python >= 3.9
* conan  >= 1.62.0
* cmake  >= 3.10

## Build

```bash
cabal build
```

## Usage

The application provides several interfaces for different use cases.

### CLI
Runs the game in the terminal for local interaction.

```bash
cabal run go-haskell -- cli
```

#### Key Bindings

| Key | Command     |
|:----|:------------|
| Esc | Exit        |
| f   | Place Stone |
| g   | Pass        |
| k   | Move top    |
| l   | Move right  |
| j   | Move down   |
| h   | Move left   |

### WebSocket API
Starts the WebSocket API server.

```bash
# Default (localhost:8000)
cabal run go-haskell -- socket

# Custom Host/Port
cabal run go-haskell -- socket 0.0.0.0 9000
```

### REST API
Starts the REST API server.

```bash
# Default (localhost:8000)
cabal run go-haskell -- rest

# Custom Port
cabal run go-haskell -- rest 9000
```

## Tests

Tests are managed using Conan.

### 1. Export and Build Conan Package

```bash
conan create . go-haskell-socket-api/testing
conan profile update settings.compiler.libcxx=libstdc++11 default
```

### 2. Run Tests

The socket server must be running (usually on `ws://localhost:8000`) before executing tests.

```bash
conan test tests GoHaskellSocketApiTest/0.0.1@go-haskell-socket-api/testing
```

## Docker

### Run Socket Server

```bash
docker run -p 8000:8000 --name go-game-socket-server asattelmaier/go-game-socket-server:latest
```

### Build and Push Images

Docker images can be built and pushed to Docker Hub or Google Cloud.

#### Create Image

```bash
# Docker Hub
docker image build -t asattelmaier/go-game-socket-server:latest -f docker/socket-server/Dockerfile .

# Google Cloud
docker image build -t europe-west1-docker.pkg.dev/PROJECT_ID/go-services/go-game-socket-server:latest -f docker/socket-server/Dockerfile .
```

#### Push Image

```bash
# Docker Hub
docker push asattelmaier/go-game-socket-server:latest

# Google Cloud
docker push europe-west1-docker.pkg.dev/PROJECT_ID/go-services/go-game-socket-server:latest
```

## OS Support

Only UNIX and UNIX-like operating systems are supported.
