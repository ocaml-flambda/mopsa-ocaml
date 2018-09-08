Continuous integration framework:
- [Dockerfile](Dockerfile) to create docker image able to build mopsa
- gitlab CI script [run.sh](run.sh) to build mopsa
- TODO: gitlab CI script to test mopsa


For each push, gitlab will run perform a CI job, which will:
- Create a container based on the mopsa-build docker image.
- Copy the repository in /home/mopsa.
- Run the [run.sh](run.sh) script.


# Docker image

The image has a minimal environment based on ubuntu:latest with OCaml, Opam, and all the dependencies required to install mopsa installed.
The image also has a mopas user, under which all commands are run.

The image is installed on the mopsa GitLab server.

If the dependencies change, you should update the image:
- edit the [Dockerfile](Dockerfile)
- log on the GitLab server
- recreate the image by launching, in this directory:
```
docker build -t mopsa-build .
```

Note (re)creating the image is slow.


# Scripts

The overall CI configuration script is in [../.gitlab-ci.yml](../.gitlab-ci.yml).

For now, it only launches [run.sh](run.sh).


