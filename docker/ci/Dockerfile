# Docker image to compile MOPSA on Ubuntu
# Build with: docker build -t mopsa-build .
#
###############################################

FROM ubuntu:23.10


# configuration: required packages
##

ENV APT_DEPS build-essential opam clang git pkg-config libgmp-dev libmpfr-dev \
             llvm-16-dev libclang-16-dev libclang-cpp16-dev clang-16  \
             python3-dev python3-setuptools python3-pip python3-requests \
             rsync m4 curl wget adduser less emacs-nox

ENV OPAM_DEPS dune apron zarith menhir yojson

ARG DEBIAN_FRONTEND=noninteractive

ENV TZ=Europe/Paris

# install system packages as root
# create a mopsa user
##

ENV TERM xterm-256color

RUN \
    apt-get update && \
    apt-get install --no-install-recommends -y $APT_DEPS && \
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
    opam switch create 4.14.0 && \
    eval $(opam env) && \
    opam update -y && \
    opam install -y -j 8 $OPAM_DEPS
