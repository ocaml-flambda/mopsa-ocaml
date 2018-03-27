# MOPSA

MOPSA stands for *modular and open platform for static analysis*.
It aims at making the development/use of static analyzers less painful.

For the moment, MOPSA can analyze programs written in C and Python.

## Dependencies

* ocaml (version >= 4.04.0)
* apron
* clang (version 4.x)
* zarith
* menhir
* yojson
* ocp-pack-split
* gmp
* mpfr
* camlidl

## Installation 

### Debian-based distributions

```bash
sudo apt install build-essential m4 opam llvm-4.0-dev libclang-4.0-dev libgmp-dev libmpfr-dev
sudo ln -s /usr/bin/llvm-config-4.0 /usr/bin/llvm-config
opam init
eval `opam config env`
opam switch 4.04.0
eval `opam config env`
opam install apron ocp-pack-split zarith menhir yojson
make

```

## Usage

```bash
./analyzer/bin/mopsa-c foo.c
./analyzer/bin/mopsa-python -debug=python.flows._ foo.py
./analyzer/bin/mopsa-c -test=true -debug=_unit_test_summary analyzer/tests/int_tests.c
```
