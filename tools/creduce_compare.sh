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

USAGE="
Usage: creduce_compare.sh file.c config1.json regex1 config2.json regex2

Used to reduce file.c when noticing analysis discrepancies between the results of the analyses based on config1.json (resp. config2.json), through a grep using regex1 (resp; regex2.json)

Example: you notice a soundness issue on program 182.c:
- the analysis is sound with \`cell-itv.json\`, it displays \"error: Assertion failure\"
- the analysis is unsound with \`cell-itv-excluded-powerset.json\`, it displays \"No alarm\"

You then run:
  creduce_compare.sh 182.c cell-itv.json \"error: Assertion failure\" cell-itv-excluded-powerset.json \"No alarm\"

Cvise will automatically reduce the example.

(Cf https://gitlab.com/mopsa/mopsa-analyzer/-/issues/182, issue is present in revision b32b084f56f2ea4f5e2f03a2528f3cdf889040a1)
"

if [ $# -ne 5 ] ; then
    echo "$USAGE";
    exit 0
fi

export FILE=$1
grep -q "_mopsa_assert_unreachable();" $FILE
export FORCE_ASSERT_UNREACH=$?
export CONF1=$2
export REGEX1="$3"
export CONF2=$4
export REGEX2="$5"
cvise $(dirname "$0")/creduce_compare_util.sh $FILE | tee output

if $(grep -q "cannot run because the interestingness test does not return" output); then
    echo -e "\n=> The interestingness failed, probably the regexps are not matching?\nPlease note that if you want to try creduce_compare_util.sh, you will need to export variables:\n  export FILE=$FILE FORCE_ASSERT_UNREACH=$FORCE_ASSERT_UNREACH CONF1=$CONF1 REGEX1=\"$REGEX1\" CONF2=$CONF2 REGEX2=\"$REGEX2\" ";
fi;
