FROM ubuntu:20.10

COPY dist/go-haskell

WORKDIR /opt/go-haskell

RUN mkdir log \
    && touch log/access.log \
    && touch log/error.log

CMD ["./go-haskell", "rest"]
