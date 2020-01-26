# Docker image to test Mospa on Ubuntu
# Build with: docker build -t mopsa-build .
#
###############################################

FROM ubuntu:latest


# configuration: required packages
##

ENV APT_DEPS build-essential lsb-release m4 llvm-6.0-dev libclang-6.0-dev \
    	     clang-6.0 libgmp-dev libmpfr-dev zlib1g-dev pkg-config wget  \
	     unzip bubblewrap git
ENV OPAM_SWITCH 4.08.0
ENV OPAM_DEPS apron zarith menhir yojson javalib


# install system packages as root
# create a mopsa user
##

ENV TERM xterm-256color

RUN \
    apt-get update && \
    apt-get install -y $APT_DEPS && \
    adduser --disabled-password --gecos 'Mopsa' mopsa


# install opam
##

COPY install-opam.sh /tmp
RUN sh /tmp/install-opam.sh
    

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
    opam switch create -y -j 8 $OPAM_SWITCH && \
    eval $(opam env) && \
    opam install -y -j 8 $OPAM_DEPS && \
    echo && \
    echo "All done!" && \
    uname -a && \
    cat /etc/debian_version && \    
    lsb_release -d && \
    echo opam `opam --version` && \
    ocamlc -v
