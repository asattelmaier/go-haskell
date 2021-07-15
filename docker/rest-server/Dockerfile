FROM ubuntu:20.10

# TODO: Find a better solution for accessing the build product.
# Currently the location to the build product can not be configured.
# See: https://github.com/haskell/cabal/issues/3545
COPY dist-newstyle/build/x86_64-linux/ghc-8.10.2/go-haskell-0.0.0.1/x/go-haskell/build/go-haskell/ /opt/go-haskell

WORKDIR /opt/go-haskell

RUN mkdir log \
    && touch log/access.log \
    && touch log/error.log

RUN useradd -ms /bin/bash go

RUN chown go log/access.log log/error.log go-haskell

RUN chmod +x go-haskell

USER go

ENTRYPOINT ["./go-haskell", "rest"]

