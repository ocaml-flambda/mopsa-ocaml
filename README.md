# MOPSA

MOPSA stands for *Modular and Open Platform for Static Analysis*. It aims at making the development/use of static analyzers less painful.

More specifically, MOPSA is a generic framework for building sound static analyzer based on the theory of abstract interpretation.
It features a modular architecture for supporting different kinds of languages, iterators and abstractions.

For the moment, MOPSA can analyze only programs written in C and Python.

## Dependencies

* ocaml (version >= 4.05.0)
* apron
* clang (version >= 4.x)
* zarith
* menhir
* yojson
* ocp-pack-split
* gmp
* mpfr
* camlidl

## Installation 

### DEB-based distributions

Tested on Ubuntu 16.04:

```bash
sudo apt install build-essential m4 opam llvm-5.0-dev libclang-5.0-dev libgmp-dev libmpfr-dev
opam init
eval `opam config env`
opam install apron ocp-pack-split zarith menhir yojson
make

```

### RPM-based distributions

Tested on Fedora 27:

```bash
sudo dnf install git m4 redhat-rpm-config patch opam clang-devel-4.0.? llvm-devel-4.0.? gmp-devel mpfr-devel
opam init
eval $(opam config env)
opam install apron ocp-pack-split zarith menhir yojson
make

```


## Usage

```bash
./scripts/mopsa-c foo.c
./scripts/mopsa-python -debug=python.flows._ foo.py
./scripts/mopsa-c -unittest -debug=_unittest_summary analyzer/tests/c/int_tests.c
```
