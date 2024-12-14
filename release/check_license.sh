#!/bin/bash

##############################################################################
#                                                                            #
#  This file is part of MOPSA, a Modular Open Platform for Static Analysis.  #
#                                                                            #
#  Copyright (C) 2017-2019 The MOPSA Project.                                #
#                                                                            #
#  This program is free software: you can redistribute it and/or modify      #
#  it under the terms of the GNU Lesser General Public License as published  #
#  by the Free Software Foundation, either version 3 of the License, or      #
#  (at your option) any later version.                                       #
#                                                                            #
#  This program is distributed in the hope that it will be useful,           #
#  but WITHOUT ANY WARRANTY; without even the implied warranty of            #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             #
#  GNU Lesser General Public License for more details.                       #
#                                                                            #
#  You should have received a copy of the GNU Lesser General Public License  #
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.     #
#                                                                            #
##############################################################################


# Check that the files in the source tree contain the License text

# keyword that must occur in the source file at least once
KEY="GNU \(Lesser\|Library\) General Public License"

# files to check
MLFILES=`find .. -name \*.ml\*`
CFILES=`find .. -name \*.c`
HFILES=`find .. -name \*.h`
CCFILES=`find .. -name \*.cc`
PYFILES=`find .. -name \*.py | grep -v tests | grep -v benchmarks`
SHELLFILES=`find .. -name \*.sh`
MAKEFILES="`find .. -name \*.mk` `find .. -name \*.in | grep -v META.in` `find .. -name \*.ac` `find .. -name Makefile` ../configure.ac"
ALL="$MLFILES $CFILES $HFILES $CCFILES $PYFILES $MAKEFILES $SHELLFILES"
FILES=`echo "$ALL" | grep -v /parsers/python/ | grep -v /_build/ | grep -v /lib/ | grep -v /benchmarks/ | grep -v /tests/ | grep -v /ci/ | grep -v /doc/`
echo "Looking for files missing the text '$KEY'."
echo "Checking" `echo $FILES | wc -w` "file(s)."

OUT=`grep -L "$KEY" $FILES`

if test $? != 1 && [ -n "$OUT" ]
then
    echo "ERROR: some files are missing the License text."
    echo "$OUT"
    exit 1
fi

echo "OK, all your files have the License text."
