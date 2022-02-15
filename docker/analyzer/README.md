# Contents

The Docker image has MOPSA compiled from the master branch on the GitLab repository.
This removes the need for the user to compile MOPSA from source and can be useful for Continuous Integration.


# Getting the image

The Docker image is available on Docker Hub at https://hub.docker.com/r/mopsa/mopsa-analyzer
```
docker pull mopsa/mopsa-analyzer
```

You can also rebuild the Docker image locally with:
```
docker build -t mopsa/mopsa-analyzer .
```

# Using the image


## Running unit tests

To run the unit tests included in the MOPSA distribution on the Docker image, type:
```
docker run mopsa/mopsa-analyzer /bin/bash -c "cd mopsa-analyzer && make tests"
```

## Running a software analysis inside the Docker image

We show how to download and analyze a program entirely on the Docker image, without altering the host file system.
We use GNU `time` as example program to analyze.
We first create a Docker container with the MOPSA analyzer and log in:
```
docker run --rm -it mopsa/mopsa-analyzer
```
The software is downloaded and analyzed from within the container as follows:
```
cd workspace
wget https://ftp.gnu.org/gnu/time/time-1.9.tar.gz
tar xaf time-1.9.tar.gz
cd time-1.9
./configure CC=clang
mopsa-build make
mopsa-c mopsa.db
```
The result is output on the console.


## Analyzing a host project using the Docker image

We now show how to analyze a software with source on the host and keep the JSON result file on the host, still using the Docker image to perform the analysis itself.

On our example analyzing GNU `time`, we first download it on the host as:
```
mkdir -p workspace
cd workspace
wget https://ftp.gnu.org/gnu/time/time-1.9.tar.gz
tar xaf time-1.9.tar.gz
```
We now log on a new docker container, sharing the `workspace` directory with the host:
```
docker run --rm -it --mount type=bind,src=`pwd`,target=/home/mopsa/workspace mopsa/mopsa-analyzer
```
Once in the container, the analysis is run as:
```
cd workspace/time-1.9
./configure CC=clang
mopsa-build make
mopsa-c mopsa.db -format=json -output=../result.json -no-warning
```
After exiting the container, the analysis results remain available on the host as `result.json` in the `workspace` directory.
