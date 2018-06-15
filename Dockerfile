FROM debian:9.4

WORKDIR /home

RUN apt-get update && \
    apt-get -y install build-essential haskell-platform sdcc

# install dependencies as a seperate step because
# this takes quite a long time and changes less
# frequently
COPY cabal.config *.cabal /home/
RUN cabal update \
    && cabal install --only-dependencies

# build firmware first - it's required by the main tool
COPY firmware /home/firmware/
RUN make -C firmware

COPY *.hs LICENSE /home/
RUN cabal install --bindir=/usr/local/bin
