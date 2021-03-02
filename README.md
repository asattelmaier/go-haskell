# Go Haskell



## Prerequisites

* ghc   >= 8.4.4
* cabal >= 1.10
* conan >= 1.33.1
* cmake >= 2.8.12



## Build

```bash
cabal build
```



## Run CLI Game

```bash
cabal run -- -m cli
```



## Run Tests

```
conan create . go-haskell-json-api/testing
conan test tests GoHaskellJsonApiTest/0.0.1@go-haskell-json-api/testing
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




## OS Support

Only UNIX and UNIX-like operating systems are supported.

