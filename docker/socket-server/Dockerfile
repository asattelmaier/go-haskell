FROM ubuntu:24.04



#==============================
# Copy binary and assets
#==============================
COPY --chmod=500 dist-newstyle /opt/go-haskell/dist
RUN find /opt/go-haskell/dist -type f -executable -name "go-haskell" -exec cp {} /opt/go-haskell/go-haskell \;
RUN rm -rf /opt/go-haskell/dist



#==============================
# Create log files
#==============================
WORKDIR /opt/go-haskell

RUN mkdir log \
    && touch log/access.log \
    && touch log/error.log



#==============================
# Create user
#==============================
RUN useradd -ms /bin/bash go-haskell \
  && chown -R go-haskell:go-haskell /opt/go-haskell

USER go-haskell

ENTRYPOINT ["./go-haskell", "socket"]

