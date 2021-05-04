# Contents

The image has a minimal environment based on `ubuntu` with OCaml, Opam, and all the dependencies required to _compile_ MOPSA installed.
However, it _does not_ have MOPSA already installed.

The image has a `mopsa` user, under which all commands are run.


# Getting the image

The image is available on Docker Hub at https://hub.docker.com/r/mopsa/mopsa-build
```
docker pull mopsa/mopsa-build
```

You can also rebuild the image locally with:
```
docker build -t mopsa-build .
```

# CI

The image (from Docked Hub) is used in our GitLab CI to check that MOPSA builds and passes basic unit tests.
See the `.gitlab-ci.yml` file at the root of the GitLab project.
A MOPSA fork on GitLab will be able to run the CI, as long as you have GitLab Runners configured.
