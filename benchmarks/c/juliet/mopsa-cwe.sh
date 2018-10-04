#!/bin/bash

MOPSADIR=$1
directory=$(dirname $2)
casename=$(basename $2 .c)

files=$(find ${directory} -name "${casename}*.c" | sort -nr)
good="${casename}_good"
bad="${casename}_bad"

MOPSA="${MOPSADIR}/bin/mopsa-c -format=json -I ${MOPSADIR}/analyzer/stubs/c -I ${MOPSADIR}/analyzer/stubs/c/juliet ${MOPSADIR}/analyzer/stubs/c/mopsa_libc.c ${MOPSADIR}/analyzer/stubs/c/juliet/std_testcase.c ${MOPSADIR}/analyzer/stubs/c/juliet/std_testcase_io.c"

good_output=$(${MOPSA} -c-entry "${good}" ${files})
bad_output=$(${MOPSA} -c-entry "${bad}" ${files})

echo "{"
echo " \"case\": \"${casename}\","
echo " \"good\": ${good_output},"
echo " \"bad\": ${bad_output}"
echo "},"
