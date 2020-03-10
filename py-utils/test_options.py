#!/usr/bin/env python3
import time
import subprocess
import collections
import os
import re

file_dir = "../../pybenchs/"
gctime_str = "Total gc time : "
ocaml_gc_stats = "v=0x400"


results = {}

def run_mopsa(mopsa_name, mopsa_options, file_name, runs=1, cwd=None, file_dir=""):
    rt = 0
    for i in range(runs):
        start = time.time()
        proc = subprocess.Popen([mopsa_name, file_dir + file_name] + mopsa_options, stdout=subprocess.PIPE, stderr=subprocess.PIPE, env=dict(os.environ, OCAMLRUNPARAM=ocaml_gc_stats), cwd=cwd)
        stdout, stderr = proc.communicate()
        stop = time.time()
        rt += stop - start
    stdout = stdout.decode()
    stderr = stderr.decode()
    with open("logs/" + mopsa_name.replace("..", "_").replace("/", "_") + "_".join(mopsa_options) + "_" + file_name, 'w+') as f:
        f.write(stdout)
    alarms = re.findall("Uncaught Python exception: ([a-zA-Z-_][a-zA-Z0-9_-]*)", stdout)
    usedmem = int(int(stderr.split()[-3]) * 8 / 10**6) # this should be ocaml's top_heap_words, now in MB
    return rt / runs, alarms, stdout, usedmem

def f(percent=100, global_options=None):
    if global_options is None:
        global_options = []
    options = { "GC": ["-gc", f"-gc-percent={percent}", "-hook gctest"] }
    print(f"GC percent: {percent}, Global options: {global_options}")
    base_config = { "Type Analysis": "../bin/mopsa-python-types",
                "Value Analysis": "../bin/mopsa-python" }
    files = ["bm_fannkuch.py", "bm_float.py", "bm_spectral_norm.py",
             "bm_nbody.py", "bm_chaos.py", "bm_raytrace.py",
             "bm_scimark.py", "bm_richards.py",
             "bm_unpack_sequence.py", "bm_go.py", "bm_hexiom.py",
             "bm_regex_v8.py" ]
    fpp_files = ["processInput.py", "choose.py"]
    s = "| Filename              "
    for config_name, config_str in base_config.items():
        for option_name, option_str in options.items():
            name1 = f"{config_name}"
            name2 = f"{name1} + {option_name}"
            s = f"{s}| {name1:25} | {name2:33} | improvement (s, MB) "

    print(s + "|")

    total_noopt = collections.defaultdict(int)
    total_opt = collections.defaultdict(int)
    for f in files:
        print(f"| {f:21} ", end="", flush=True)
        try:
            for config_name, config_str in base_config.items():
                for option_name, option_str in options.items():
                    t_noopt, errs_noopt, _, usedmem_noopt = run_mopsa(config_str, global_options, f, file_dir=file_dir)
                    t_opt, errs_opt, stdout, usedmem_opt = run_mopsa(config_str, global_options + option_str, f, file_dir=file_dir)
                    errs_opt_ty = errs_opt.count("TypeError") + errs_opt.count("AttributeError")
                    errs_opt_val = len(errs_opt) - errs_opt_ty
                    errs_noopt_ty = errs_noopt.count("TypeError") + errs_noopt.count("AttributeError")
                    errs_noopt_val = len(errs_noopt) - errs_noopt_ty
                    assert(errs_noopt == errs_opt)
                    gc_time = float(stdout[stdout.find(gctime_str)+len(gctime_str):].split()[0])

                    total_noopt[config_name, option_name] += t_noopt
                    total_opt[config_name, option_name] += t_opt
                    results[f, config_name, option_name, True] = (t_opt, errs_opt, usedmem_opt)
                    results[f, config_name, option_name, False] = (t_noopt, errs_noopt, usedmem_noopt)
                    err = (100 * (t_noopt - t_opt) / t_noopt).__round__()
                    mem = (100 * (usedmem_noopt - usedmem_opt) / usedmem_noopt).__round__()
                    print(f"| {t_noopt:7.3f}s ({errs_noopt_ty:>2}, {errs_noopt_val:>2}, {usedmem_noopt:>4}MB) | {t_opt:7.3f}s ({gc_time:5.3f}s, {errs_opt_ty:>2}, {errs_opt_val:>2}, {usedmem_opt:>4}MB) |    {err:3}%    {mem:3}%     ", end="", flush=True)
            print("|")
        except KeyboardInterrupt:
            print()
            continue

    cwd = os.getcwd()
    fpp_dir = file_dir + "pathpicker/"
    for f in fpp_files:
        print(f"| {f:21} ", end="", flush=True)
        try:
            for config_name, config_str in base_config.items():
                for option_name, option_str in options.items():
                    t_noopt, errs_noopt, _, usedmem_noopt = run_mopsa(cwd + "/" + config_str, global_options, f, cwd=fpp_dir)
                    t_opt, errs_opt, stdout, usedmem_opt = run_mopsa(cwd + "/" + config_str, global_options + option_str, f, cwd=fpp_dir)
                    errs_opt_ty = errs_opt.count("TypeError") + errs_opt.count("AttributeError")
                    errs_opt_val = len(errs_opt) - errs_opt_ty
                    errs_noopt_ty = errs_noopt.count("TypeError") + errs_noopt.count("AttributeError")
                    errs_noopt_val = len(errs_noopt) - errs_noopt_ty
                    assert(errs_noopt == errs_opt)
                    gc_time = float(stdout[stdout.find(gctime_str)+len(gctime_str):].split()[0])

                    total_noopt[config_name, option_name] += t_noopt
                    total_opt[config_name, option_name] += t_opt
                    results[f, config_name, option_name, True] = (t_opt, errs_opt, usedmem_opt)
                    results[f, config_name, option_name, False] = (t_noopt, errs_noopt, usedmem_noopt)
                    err = (100 * (t_noopt - t_opt) / t_noopt).__round__()
                    mem = (100 * (usedmem_noopt - usedmem_opt) / usedmem_noopt).__round__()
                    print(f"| {t_noopt:7.3f}s ({errs_noopt_ty:>2}, {errs_noopt_val:>2}, {usedmem_noopt:>4}MB) | {t_opt:7.3f}s ({gc_time:5.3f}s, {errs_opt_ty:>2}, {errs_opt_val:>2}, {usedmem_opt:>4}MB) |    {err:3}%    {mem:3}%     ", end="", flush=True)
            print("|")
        except KeyboardInterrupt:
            print()
            continue
        subprocess.Popen(["bash", "toggle_usage.sh"], cwd=fpp_dir, stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()

    with open(f"results_{percent}_{'_'.join(global_options)}", "w+") as f:
        f.write(str(results))

    total = "Total"
    print(f"| {total:21} ", end="", flush=True)
    for config_name, config_str in base_config.items():
        for option_name, option_str in options.items():
            err = (100 * (total_noopt[config_name, option_name] - total_opt[config_name, option_name]) / total_noopt[config_name, option_name]).__round__()
            print(f"| {total_noopt[config_name, option_name]:24.3f}s | {total_opt[config_name, option_name]:32.3f}s | {err:18.2f}% ", end="", flush=True)
    print("|\n")


if __name__ == "__main__":
#    for x in reversed([1, 12, 25, 37, 50, 62, 75, 87, 100]):
    f(100, ["-default-alloc-pol=all"])
    f(100, ["-default-alloc-pol=range"])
    f(100, ["-default-alloc-pol=callstack"])
    f(100, ["-default-alloc-pol=range_callstack"])
