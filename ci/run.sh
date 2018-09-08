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

./scripts/mopsa-universal analyzer/tests/universal/int_tests.u

echo "Success"
