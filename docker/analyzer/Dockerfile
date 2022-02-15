# Docker image with MOPSA installed on Ubuntu
###############################################

FROM mopsa/mopsa-build:latest

ENV PATH="/home/mopsa/bin:${PATH}"

RUN \
    eval `opam config env` && \
    git clone --progress https://gitlab.com/mopsa/mopsa-analyzer.git && \
    cd mopsa-analyzer && \
    ./configure --prefix /home/mopsa && \
    make -j && \
    make install && \
    make tests && \
    mopsa -v && \
    cd /home/mopsa && \
    mkdir workspace

WORKDIR /home/mopsa

