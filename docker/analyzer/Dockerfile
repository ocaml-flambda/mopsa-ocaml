# Docker image with MOPSA installed on Ubuntu
###############################################

FROM mopsa/mopsa-build:latest

ENV PATH="/home/mopsa/bin:${PATH}"

WORKDIR /home/mopsa

RUN \
    eval `opam config env` && \
    opam env >> .profile && \
    echo ". /home/mopsa/banner.sh" >> .profile && \
    opam env >> .bashrc && \
    echo ". /home/mopsa/banner.sh" >> .bashrc && \
    git clone --progress https://gitlab.com/mopsa/mopsa-analyzer.git && \
    cd mopsa-analyzer && \
    ./configure --prefix /home/mopsa && \
    make -j && \
    make install && \
    make tests && \
    mopsa -v && \
    cd /home/mopsa && \
    mkdir workspace

COPY banner.sh /home/mopsa/

# avoid having to run `eval $(opam env)` at each `docker run`
#ENTRYPOINT ["opam", "exec", "--"]
#CMD ["/bin/bash", "--login"]
