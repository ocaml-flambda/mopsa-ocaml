#!/bin/bash

# This file is part of MOPSA, a Modular Open Platform for Static Analysis.
#
# Copyright (C) 2024 The MOPSA authors.
#
# SPDX-License-Identifier: LGPL-3.0-or-later
#
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by the Free
# Software Foundation, either version 3 of the License, or (at your option) any
# later version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
# details.
#
# You should have received a copy of the GNU Lesser General Public License along
# with this program. If not, see <http://www.gnu.org/licenses/>.

# This file is intended to be used as an oracle to reduce failing testcases.

## DEPENDENCIES
# You need to have either cvise or creduce installed

# On ubuntu, `apt install cvise creduce` works

# Note that we have encountered cases where creduce works and not cvise, and
# conversely, so it's best to test both

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
MOPSA_COMMAND=${MOPSA_COMMAND:-"mopsa-c -config=c/markertail-cell-string-length-pack-rel-itv-congr.json -c-init-memset-threshold=5 -trace-partition-tail-length=1 -additional-stubs=c/sv-comp.c -use-stub=reach_error,fread_unlocked,fread,strndup,xstrndup -no-warning -ccopt=-fbracket-depth=2048 -stub-ignore-case=malloc.failure,malloc.empty -c-check-signed-implicit-cast-overflow=false -c-check-unsigned-implicit-cast-overflow=false"}
# 3) fill MOPSA_ERR_STRING below.
#    It should contain the crashing behavior you want to reproduce (for example "man.get called on a non-singleton map")
#    The result will be grepped against the whole output of MOPSA_COMMAND (on both stderr+stdout)
MOPSA_ERR_STRING=${MOPSA_ERR_STRING:-"man.get called on a non-singleton map"}
# 4) save this file, and run (from the root of Mopsa):
# $ cvise -n $(nproc) --sllooww ./tools/reducer-oracle.sh small.c
# the sllooww option is not mandatory, it is supposed to perform better reductions
FILE_TO_REDUCE=${FILE_TO_REDUCE:-small.c}

# stubs need to be passed AFTER the analyzed program to Mopsa hence the different env variable
MOPSA_STUBS=${MOPSA_STUBS:-""}

## CURRENT LIMITATIONS
# Can only reduce a single C file.
# If you have a multifile C project, the best approach would be to preprocess the source code, for example using cil (or implement Mopsa issue #110 to have this built-in).
# Other languages: to try

## ACTUAL ORACLE
# small.c is the initial file and the one that be will be reduced
# we first check it's syntactically valid C with clang
(clang -c $FILE_TO_REDUCE >/dev/null 2>&1 || (echo "Clang validation failed on $FILE_TO_REDUCE!" && exit 1)) &&
# not sure the timeout is really needed since we supposedly reduce the program's length, at least.
# || true to not stop if mopsa finds alarms or crashes
( $MOPSA_COMMAND $FILE_TO_REDUCE $MOPSA_STUBS 2>&1 > stdout || true ) &&
# this is the behavior we want to reproduce
grep "$MOPSA_ERR_STRING" stdout
