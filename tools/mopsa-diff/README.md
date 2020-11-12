About
=====

`mopsa-diff` compares the JSON output of two `mopsa` analyses.

Usage
=====

```bash
$ mopsa-diff <path-to-old-json> <path-to-new-json> # compare two json files
$ mopsa-diff <path-to-old-json-dir> <path-to-new-json-dir> # compare all json files in two directories
```

Main Options
============

- `--ignore-time` to avoid comparing analysis time.
- `--summary` to print a summary of the comparison.
- `--regression` fail when detecting a regression.
