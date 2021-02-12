## Prerequisites
* conan >= 1.33.1
* cmake >= 3.13.4


## Compiling steps

1. Create a build directory:

```
mkdir build && cd build
```

2. Install dependencies, build and run tests

```
conan install .. && cmake .. && cmake --build . -- -j8 && ctest
```
