#!/usr/bin/env python3
import time
import subprocess
import collections

file_dir = "/home/raphael/these/pybenchs/"
gctime_str = "Total gc time : "

def run_mopsa(mopsa_name, mopsa_options, file_name, runs=1):
    rt = 0
    for i in range(runs):
        start = time.time()
        proc = subprocess.Popen([mopsa_name, file_dir + file_name] + mopsa_options, stdout=subprocess.PIPE, stderr=subprocess.PIPE)
        stdout, stderr = proc.communicate()
        stop = time.time()
        rt += stop - start
    return rt / runs, stdout

def f(percent=100):
    options = { "GC": ["-gc", f"-gc-percent={percent}", "-hook gctest"] }
    print(f"GC percent: {percent}")
    base_config = { "Type Analysis": "../bin/mopsa-python-types",
                "Value Analysis": "../bin/mopsa-python" }
    files = ["bm_chaos.py", "bm_deltablue.py",
             "bm_fannkuch.py", "bm_float.py", "bm_go.py"
             "bm_hexiom.py", "bm_nbody.py", "bm_nqueens.py",
             "bm_pidigits.py", "bm_raytrace.py", "bm_regex_v8.py",
             "bm_richards.py", "bm_scimark.py",
             "bm_spectral_norm.py", "bm_unpack_sequence.py"]
    s = "| Filename              "
    for config_name, config_str in base_config.items():
        for option_name, option_str in options.items():
            name1 = f"{config_name}"
            name2 = f"{name1} + {option_name}"
            s = f"{s}| {name1:20} | {name2:20} | speedup "

    print(s + "|")

    total_noopt = collections.defaultdict(int)
    total_opt = collections.defaultdict(int)
    for f in files:
        print(f"| {f:21} ", end="", flush=True)
        try:
            for config_name, config_str in base_config.items():
                for option_name, option_str in options.items():
                    t_noopt, _ = run_mopsa(config_str, [], f)
                    t_opt, stdout = run_mopsa(config_str, option_str, f)

                    stdout = stdout.decode()
                    gc_time = float(stdout[stdout.find(gctime_str)+len(gctime_str):].split()[0])

                    total_noopt[config_name, option_name] += t_noopt
                    total_opt[config_name, option_name] += t_opt
                    err = 100 * (t_noopt - t_opt) / t_noopt
                    print(f"| {t_noopt:19.3f}s | {t_opt:10.3f}s ({gc_time:5.3f}s) | {err:6.2f}% ", end="", flush=True)
            print("|")
        except KeyboardInterrupt:
            print()
            continue

    total = "Total"
    print(f"| {total:21} ", end="", flush=True)
    for config_name, config_str in base_config.items():
        for option_name, option_str in options.items():
            err = 100 * (total_noopt[config_name, option_name] - total_opt[config_name, option_name]) / total_noopt[config_name, option_name]
            print(f"| {total_noopt[config_name, option_name]:19.3f}s | {total_opt[config_name, option_name]:19.3f}s | {err:6.2f}% ", end="", flush=True)
    print("|\n")


if __name__ == "__main__":
    for x in [1, 25, 50, 75, 100]:
        f(x)
