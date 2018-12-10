# Docker image to test Mospa on Ubuntu
# Build with: docker build -t mopsa-build .
#
###############################################

FROM ubuntu:latest


# configuration: required packages
##

ENV APT_DEPS build-essential lsb-release m4 opam llvm-6.0-dev libclang-6.0-dev clang-6.0 libgmp-dev libmpfr-dev zlib1g-dev
ENV OPAM_SWITCH 4.07.0
ENV OPAM_DEPS apron ocp-pack-split zarith menhir yojson javalib


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
    opam init -y && \
    opam config setup -a && \
    eval `opam config -y env` && \
    opam update -y && \
    opam switch -y -j 8 $OPAM_SWITCH && \
    eval `opam config -y env` && \
    opam install -y -j 8 $OPAM_DEPS && \
    echo && \
    echo "All done!" && \
    uname -a && \
    cat /etc/debian_version && \    
    lsb_release -d && \
    echo opam `opam --version` && \
    ocamlc -v
