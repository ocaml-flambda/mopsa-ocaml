#!/usr/bin/env python3
import time
import subprocess
import collections
import os

file_dir = "../../pybenchs/"
gctime_str = "Total gc time : "
ocaml_gc_stats = "v=0x400"

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
    try:
        nalarms = int(stdout[stdout.find("Total: ") + len("Total: "):].split()[0])
    except ValueError:
        nalarms = 0
    usedmem = int(stderr.split()[-3]) * 8 / 10**6 # this should be ocaml's top_heap_words, now in MB
    return rt / runs, nalarms, stdout, usedmem

def f(percent=100, global_options=None):
    if global_options is None:
        global_options = []
    options = { "GC": ["-gc", f"-gc-percent={percent}", "-hook gctest"] }
    print(f"GC percent: {percent}, Global options: {global_options}")
    base_config = { "Type Analysis": "../bin/mopsa-python-types",
                "Value Analysis": "../bin/mopsa-python" }
    files = []
    # "bm_chaos.py", #"bm_deltablue.py",
    # "bm_fannkuch.py", "bm_float.py", "bm_go.py",
    #          "bm_hexiom.py", "bm_nbody.py",
    #          # "bm_pidigits.py",
    #          "bm_raytrace.py", "bm_regex_v8.py",
    #          "bm_richards.py", "bm_scimark.py",
    #          "bm_spectral_norm.py", "bm_unpack_sequence.py"]
    fpp_files = ["choose.py", "processInput.py"]
    s = "| Filename              "
    for config_name, config_str in base_config.items():
        for option_name, option_str in options.items():
            name1 = f"{config_name}"
            name2 = f"{name1} + {option_name}"
            s = f"{s}| {name1:21} | {name2:30} | speedup "

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

                    gc_time = float(stdout[stdout.find(gctime_str)+len(gctime_str):].split()[0])

                    total_noopt[config_name, option_name] += t_noopt
                    total_opt[config_name, option_name] += t_opt
                    err = 100 * (t_noopt - t_opt) / t_noopt
                    print(f"| {t_noopt:7.3f}s ({errs_noopt:>2}, {usedmem_noopt:>4}MB) | {t_opt:7.3f}s ({gc_time:5.3f}s, {errs_opt:>2}, {usedmem_opt:>4}MB) | {err:6.2f}% ", end="", flush=True)
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

                    gc_time = float(stdout[stdout.find(gctime_str)+len(gctime_str):].split()[0])

                    total_noopt[config_name, option_name] += t_noopt
                    total_opt[config_name, option_name] += t_opt
                    err = 100 * (t_noopt - t_opt) / t_noopt
                    print(f"| {t_noopt:7.3f}s ({errs_noopt:>2}, {usedmem_noopt:>4}MB) | {t_opt:7.3f}s ({gc_time:5.3f}s, {errs_opt:>2}, {usedmem_opt:>4}MB) | {err:6.2f}% ", end="", flush=True)
            print("|")
        except KeyboardInterrupt:
            print()
            continue
        subprocess.Popen(["bash", "toggle_usage.sh"], cwd=fpp_dir, stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()


    total = "Total"
    print(f"| {total:21} ", end="", flush=True)
    for config_name, config_str in base_config.items():
        for option_name, option_str in options.items():
            err = 100 * (total_noopt[config_name, option_name] - total_opt[config_name, option_name]) / total_noopt[config_name, option_name]
            print(f"| {total_noopt[config_name, option_name]:18.3f}s | {total_opt[config_name, option_name]:26.3f}s | {err:6.2f}% ", end="", flush=True)
    print("|\n")


if __name__ == "__main__":
#    for x in reversed([1, 12, 25, 37, 50, 62, 75, 87, 100]):
    f(100, ["-default-alloc-pol=all"])
    f(100, ["-default-alloc-pol=range"])
    f(100, ["-default-alloc-pol=callstack"])
    f(100, ["-default-alloc-pol=range_callstack"])
