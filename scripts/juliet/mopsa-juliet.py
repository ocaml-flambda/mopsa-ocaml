#!/usr/bin/python3

import os
from subprocess import Popen, PIPE, call
import shlex
import json
import tempfile
import argparse
import re

parser = argparse.ArgumentParser(description='MOPSA launcher for Juliet testcase suite.')
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
    call("parallel --no-notice --bar %s/scripts/juliet/mopsa-juliet-case-bench :::: %s >> %s"%(args.mopsa, fp.name, args.out), shell = True)

#Close the bracket
with open(args.out, 'at') as fp:
    fp.write("{}]")


def parse_name(name):
    m = re.search("CWE(\d*).*_(good|bad)", name)
    return (int(m.group(1)), m.group(2))


def parse_result(result):
    if "alarms" in result:
        return (True, result["alarms"], result["time"])
    else:
        return (False, None, None)

cwe_alarm = {
    124: "Out of bound access",
    126: "Out of bound access",
    127: "Out of bound access",
    190: "Integer overflow",
    369: "Division by zero",
    476: "Null pointer dereference",
}

def is_cwe_alarm(alarm, cwe):
    return (cwe_alarm[cwe] == alarm["type"])

#Construct the results table
results = {}
with open(args.out, "rt") as fp:
    data = json.load(fp)
    for r in data:
        if "result" not in r:
            continue

        cwe, case = parse_name(r["name"])

        completed, alarms, time = parse_result(r["result"])

        false_positives = 0
        false_negatives = 0

        if completed:
            if case == "good":
                false_positives += len(alarms)
            elif case == "bad":
                found = False
                for a in alarms:
                    if is_cwe_alarm(a, cwe):
                        found = True
                    else:
                        false_positives += 1
                if not found:
                    false_negatives += 1

        if cwe in results:
            old = results[cwe]
        else:
            old = {
                "time": [],
                "total": 0,
                "completed": 0,
                "false_positives": 0,
                "false_negatives": 0
            }

        results[cwe] = {
            "time": old["time"] + [time] if completed else old["time"],
            "total": old["total"] + 1,
            "completed": old["completed"] + completed,
            "false_positives": old["false_positives"] + false_positives,
            "false_negatives": old["false_negatives"] + false_negatives
        }

#Print results
for cwe in results:
    print("CWE%d (%s)"%(cwe, cwe_alarm[cwe]))
    print("\tTotal: %d"%results[cwe]["total"])
    print("\tCoverage: %d%%"%(int(100 * results[cwe]["completed"]/results[cwe]["total"])))
    t = results[cwe]["time"]
    avg = sum(t)/results[cwe]["total"]
    print("\tAnalysis time (min, avg, max): %.4f/%.4f/%.4f"%(min(t), avg, max(t)))
    print("\tFalse postivies: %d"%(results[cwe]["false_positives"]))
    print("\tFalse negatives: %d"%(results[cwe]["false_negatives"]))
