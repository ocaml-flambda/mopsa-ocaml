FROM ubuntu:22.04

USER root

ENV APT_DEPS build-essential opam clang git pkg-config libgmp-dev libmpfr-dev \
             llvm-dev libclang-dev libclang-cpp-dev clang  \
             python3-dev python3-setuptools python3-pip python3-requests \
             rsync m4 curl wget libc6-dev-i386 zip

ENV OPAM_DEPS apron zarith menhir yojson

ARG DEBIAN_FRONTEND=noninteractive

ENV TZ=Europe/Paris

# install system packages as root
# create a mopsa user
##

ENV TERM xterm-256color

RUN apt-get update
RUN apt-get install --no-install-recommends -y $APT_DEPS


ENV LANG C.UTF-8
ENV LC_ALL C.UTF-8

RUN \
    opam init --disable-sandboxing -y && \
    eval $(opam env) && \
    opam update -y && \
    opam install -y -j 8 $OPAM_DEPS


ENV DEBIAN_FRONTEND=noninteractive
RUN apt-get update \
	&& apt-get install -y wget bc python3-yaml python3-pkg-resources python3-coloredlogs libseccomp2 lxcfs \
	&& rm -rf /var/lib/apt/lists/*
RUN wget https://github.com/sosy-lab/benchexec/releases/download/3.13/benchexec_3.13-1_all.deb && dpkg -i ./benchexec_*.deb && rm benchexec_*.deb
# RUN git clone https://gitlab.com/sosy-lab/benchmarking/sv-benchmarks.git
RUN git clone -b testing https://gitlab.com/rmonat/sv-benchmarks.git
