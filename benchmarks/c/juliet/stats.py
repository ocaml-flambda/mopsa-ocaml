#!/usr/bin/python3

import os
import json
import argparse
import re

parser = argparse.ArgumentParser(description='Post-precessor of Juliet benchmarks.')
parser.add_argument('json', help='path to results file')

args = parser.parse_args()


def get_cwe(case):
    m = re.search("CWE(\d*).*", case)
    return int(m.group(1))


cwe_alarm = {
    124: "Out of bound access",
    126: "Out of bound access",
    127: "Out of bound access",
    190: "Integer overflow",
    369: "Division by zero",
    476: "Null pointer dereference",
}

def is_cwe_alarm(alarm, cwe):
    return (cwe_alarm[cwe] == alarm["title"])

#Construct the results table
results = {}
with open(args.json, "rt") as fp:
    data = json.load(fp)
    for r in data:
        if len(r) == 0:
            continue

        cwe = get_cwe(r["case"])

        correct = 0
        fail = 0
        false_positives = 0
        false_negatives = 0

        time = []

        # Good case
        if r["good"]["success"]:
            time.append(r["good"]["time"])
            if len(r["good"]["alarms"]) == 0:
                correct += 1
            else:
                false_positives = len(r["good"]["alarms"])
        else:
            fail += 1

        # Bad case
        if r["bad"]["success"]:
            found = False
            time.append(r["bad"]["time"])
            for a in r["bad"]["alarms"]:
                if is_cwe_alarm(a, cwe):
                    correct += 1
                    found = True
                else:
                    false_positives += 1
            if not found:
                false_negatives += 1
        else:
            fail += 1


        if cwe in results:
            old = results[cwe]
        else:
            old = {
                "time": [],
                "total": 0,
                "correct": 0,
                "fail": 0,
                "false_positives": 0,
                "false_negatives": 0
            }

        results[cwe] = {
            "time": old["time"] + time if fail == 0 else old["time"],
            "total": old["total"] + 1,
            "correct": old["correct"] + correct,
            "fail": old["fail"] + fail,
            "false_positives": old["false_positives"] + false_positives,
            "false_negatives": old["false_negatives"] + false_negatives
        }

#Print results
for cwe in results:
    print("[CWE%d] %s:"%(cwe, cwe_alarm[cwe]))
    print("\tTotal: %d"%(results[cwe]["total"] * 2))
    print("\tCorrect: %d"%(results[cwe]["correct"]))
    print("\tFalse postivies: %d"%(results[cwe]["false_positives"]))
    print("\tFalse negatives: %d"%(results[cwe]["false_negatives"]))
    print("\tCoverage: %d%%"%(int(100 * len(results[cwe]["time"])/results[cwe]["total"])))
    t = results[cwe]["time"]
    avg = sum(t)/results[cwe]["total"]
    print("\tAnalysis time (min, avg, max, sum): %.3fs/%.3fs/%.3fs/%.3fs"%(min(t), avg, max(t), sum(t)))
