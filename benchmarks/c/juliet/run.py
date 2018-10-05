#!/usr/bin/python3

import os
from subprocess import Popen, PIPE, call
import shlex
import json
import tempfile
import argparse
import re

parser = argparse.ArgumentParser(description='Launcher of Juliet benchmarks.')
parser.add_argument('cwe', nargs='+',
                    help='list of CWE\'s to analyze (format: cwe[:subdir1,subdir2,...] ...)')
parser.add_argument('--juliet', default="/opt/juliet",
                   help='path to Juliet testcase suite')
parser.add_argument('--mopsa', default="/opt/mopsa",
                   help='path to MOPSA')
parser.add_argument('--out', default="out.json",
                   help='path to output file')

args = parser.parse_args()

#Get the directories of the target CWE's
dirs = []
for cwe in args.cwe:
    parts = cwe.split(':')
    if len(parts) == 1:
        dirs.append("CWE%s_*/*"%cwe)
    else:
        cwe = parts[0]
        for d in parts[1].split(","):
            dirs.append("CWE%s_*/%s"%(cwe, d))

#Get test cases in dirs
cases = []
for d in dirs:
    d0 = "%s/C/testcases/%s"%(args.juliet, d)
    command = """find %s -regextype posix-egrep -regex ".*[0-9]a?\.c" | sed "s/a\.c/\.c/" | sed "s/\.c//" | sort """%(d0)
    with Popen(command, shell=True, stdout=PIPE) as proc:
        out, err = proc.communicate()
        for f in out.splitlines():
            cases.append(f.decode("ascii"))

#Write first opening bracket
with open(args.out, 'wt') as fp:
    fp.write("[\n")

#Execute test cases in parallel
with tempfile.NamedTemporaryFile(mode='wt') as fp:
    fp.write("\n".join(cases))
    call("parallel --no-notice --bar %s/mopsa-cwe.sh %s :::: %s >> %s"%(
        os.path.dirname(os.path.realpath(__file__)),
        args.mopsa,
        fp.name,
        args.out
    ), shell = True)

#Close the bracket
with open(args.out, 'at') as fp:
    fp.write("{}]")
