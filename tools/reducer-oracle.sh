#!/bin/bash
# This file is intended to be used as an oracle to reduce failing testcases.

## DEPENDENCIES
# You need to have cvise, the parallel implementation of creduce, installed
# On ubuntu, apt install cvise works

## USAGE
# Let's say you have a failing mopsa analysis:
#
# $ MOPSA_COMMAND file.c
# [...]
# panic: DETAILED_MESSAGE
#
# Do the following:
# 1) move to the root of Mopsa and copy file.c to it
# 2) change MOPSA_COMMAND below.
#    It should *not* contain the C file to analyze, which will be small.c
#    Cvise creates temporary directories for its testcases, make sure paths are fully described
MOPSA_COMMAND="mopsa-c -config=c/markertail-cell-string-length-pack-rel-itv-congr.json -c-init-memset-threshold=5 -trace-partition-tail-length=1 -additional-stubs=c/sv-comp.c -use-stub=reach_error,fread_unlocked,fread,strndup,xstrndup -no-warning -ccopt=-fbracket-depth=2048 -stub-ignore-case=malloc.failure,malloc.empty -c-check-signed-implicit-cast-overflow=false -c-check-unsigned-implicit-cast-overflow=false"
# 3) if your program takes more than 10s to analyze, update the TIMEOUT_DURATION below, it should be a bit above the actual analysis time of your initial file
TIMEOUT_DURATION=10s
# 4) fill MOPSA_ERR_STRING below.
#    It should contain the crashing behavior you want to reproduce (for example "man.get called on a non-singleton map")
#    The result will be grepped against the whole output of MOPSA_COMMAND (on both stderr+stdout)
MOPSA_ERR_STRING="man.get called on a non-singleton map"
# 5) save this file, and run (from the root of Mopsa):
# $ cvise -n $(nproc) --sllooww ./tools/reducer-oracle.sh small.c
# the sllooww option is not mandatory, it is supposed to perform better reductions

## CURRENT LIMITATIONS
# Can only reduce a single C file.
# If you have a multifile C project, the best approach would be to preprocess the source code, for example using cil (or implement Mopsa issue #110 to have this built-in).
# Other languages: to try

## ACTUAL ORACLE
# small.c is the initial file and the one that be will be reduced
# we first check it's syntactically valid C with gcc
gcc -c small.c >/dev/null 2>&1 &&
# not sure the timeout is really needed since we supposedly reduce the program's length, at least.
# || true to not stop if mopsa finds alarms or crashes
timeout --foreground -v $TIMEOUT_DURATION bash -c "$MOPSA_COMMAND small.c 2>&1 > stdout || true" &&
# this is the behavior we want to reproduce
grep $MOPSA_ERR_STRING stdout
