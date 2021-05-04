# Docker image to test Mospa on Ubuntu
# Build with: docker build -t mopsa-build .
#
###############################################

FROM ubuntu:20.04


# configuration: required packages
##

ENV APT_DEPS build-essential opam llvm-10-dev libclang-10-dev \
             libclang-cpp10-dev clang-10 libgmp-dev libmpfr-dev  \
             git

ENV OPAM_DEPS apron zarith menhir yojson


# install system packages as root
# create a mopsa user
##

ENV TERM xterm-256color

RUN \
    apt-get update && \
    apt-get install -y $APT_DEPS && \
    adduser --disabled-password --gecos 'Mopsa' mopsa


# log in as mopsa
#

USER mopsa
WORKDIR /home/mopsa
ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8


# install OCaml dependencies with opam as mopsa user
##

RUN \
    opam init --disable-sandboxing -y && \
    eval $(opam env) && \
    opam update -y && \
    opam install -y -j 8 $OPAM_DEPS
