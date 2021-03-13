FROM haskell:latest

RUN apt-get update && apt-get install -y \
  cmake \
  python3-pip

RUN pip3 install conan

ENTRYPOINT ["go-haskell"]

