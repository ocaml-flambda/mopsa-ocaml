# Docker image to compile MOPSA on Ubuntu
# Build with: docker build -t mopsa-build .
#
###############################################

FROM ubuntu:24.04


# configuration: required packages
##

ENV APT_DEPS build-essential opam clang git pkg-config libgmp-dev libmpfr-dev \
             llvm-dev libclang-dev libclang-cpp-dev clang \
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
    adduser --disabled-password --gecos 'Mopsa' mopsa && \
    adduser mopsa sudo

# the multilanguage analysis currently requires python with version <= 3.11
RUN \
    apt-get install -y software-properties-common && \
    add-apt-repository -y ppa:deadsnakes/ppa &&  \
    apt update && \
    apt install -y python3.11 python3.11-dev && \
    update-alternatives --install /usr/bin/python3 python3 /usr/bin/python3.11 1

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
