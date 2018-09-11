# Contents

Continuous integration framework:
- [Dockerfile](Dockerfile) to create docker image able to build Mopsa
- CI script [run.sh](run.sh) to build mopsa
- TODO: CI script to test mopsa


# Workflow

For each push, gitlab will run perform a CI job, which will:
- create a container based on the `mopsa-build` docker image
- check out the latest repository into `/home/mopsa`
- run the [run.sh](run.sh) script as user `mopsa`


# Docker image

The image has a minimal environment based on `ubuntu:latest` with OCaml, Opam, and all the dependencies required to compile Mopsa installed.
The image also has a `mopas` user, under which all commands are run.
The image is already installed on the GitLab server.

If the dependencies change, you should update the image:
- edit the [Dockerfile](Dockerfile)
- log on the GitLab server
- recreate the image by launching, in this directory:

```
docker build -t mopsa-build .
```

Note: (re)creating the image is slow.


# Scripts

The overall CI configuration script is in [../.gitlab-ci.yml](../.gitlab-ci.yml).

For now, it only launches [run.sh](run.sh).


