# Go Haskell



## Prerequisites

* ghc   >= 8.10.4
* cabal >= 2.4
* conan >= 1.33.1
* cmake >= 2.8.12



## Build

```bash
cabal build
```



## Run CLI Game

```bash
cabal run :go-haskell -- cli
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
conan create . go-haskell-json-api/testing
```

Run Tests:

```bash
conan test tests GoHaskellJsonApiTest/0.0.1@go-haskell-json-api/testing
```



## Create REST Server Container

You must first build the project.

Create Image:

```bash
docker image build -t go-haskell-rest-server -f docker/rest-server/Dockerfile .
```

Run Container:

```bash
docker run -p 8000:8000 --name go-haskell-rest-server go-haskell-rest-server:latest
```



## OS Support

Only UNIX and UNIX-like operating systems are supported.

