# Go Haskell



## Prerequisites

* ghc    == 8.10.4
* cabal  >= 3.4
* python == 3.9
* conan  == 1.60.1
* cmake  >= 2.8.12



## Build

```bash
cabal build
```



## Run CLI Game

```bash
cabal run go-haskell -- cli
```



## CLI Commands

| Key | Command     |
|:----|:------------|
| Esc | Exit        |
| f   | Place Stone |
| g   | Pass        |
| k   | Move top    |
| l   | Move right  |
| j   | Move down   |
| h   | Move left   |



## Run Tests

Export and build conan package:

```bash
conan create . go-haskell-socket-api/testing
conan profile update settings.compiler.libcxx=libstdc++11 default
```

Run Tests:

```bash
conan test tests GoHaskellSocketApiTest/0.0.1@go-haskell-socket-api/testing
```


## Run Socket Server Container

```bash
docker run -p 8000:8000 --name go-game-socket-server asattelmaier/go-game-socket-server:latest
```



## Update Socket Server Image

You must first build the project.

Create Image:

```bash
docker image build -t asattelmaier/go-game-socket-server:latest -f docker/socket-server/Dockerfile .
```

Push Image:

```bash
docker push asattelmaier/go-game-socket-server:latest
```



## OS Support

Only UNIX and UNIX-like operating systems are supported.

