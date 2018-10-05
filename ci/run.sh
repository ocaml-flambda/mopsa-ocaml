#!/bin/bash

# Simple script launched by gitlab continuous integration
#
##


# Compile
##

eval `opam config env`
make
if [ ! -x bin/mopsa ]
then
    echo "*** BUILD FAILED ***"
    exit 1
fi

# Test (TODO)
##

#make -C analyzer universal-tests
make -C analyzer c-tests
#make -C analyzer python-tests

echo "Success"
