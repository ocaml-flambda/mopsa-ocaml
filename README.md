# MOPSA

MOPSA stands for *Modular and Open Platform for Static Analysis*. It aims at making the development/use of static analyzers less painful.

More specifically, MOPSA is a generic framework for building sound static analyzer based on the theory of abstract interpretation.
It features a modular architecture for supporting different kinds of languages, iterators and abstractions.

For the moment, MOPSA can analyze only programs written in C and Python.


## License

Unless explicitly specified, the components of the MOPSA software are distributed under the terms of the GNU Lesser General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
See the accompanying COPYING file, or [http://www.gnu.org/licenses/](http://www.gnu.org/licenses/).

The documentation and example files of the MOPSA software are distributed under a Creative Commons Attribution-ShareAlike 4.0 International License. See [https://creativecommons.org/licenses/by-sa/4.0/](https://creativecommons.org/licenses/by-sa/4.0/).


## Dependencies

* ocaml (version >= 4.07.1)
* apron
* clang (version >= 5.x)
* zarith
* menhir
* yojson
* gmp
* mpfr
* camlidl
* zlib

## Installation

### DEB-based distributions

Tested on Ubuntu 16.04:

```shell
sudo apt install build-essential m4 opam clang-5.0 llvm-5.0-dev libclang-5.0-dev libgmp-dev libmpfr-dev zlib1g-dev
opam init
opam switch 4.07.1
eval `opam config env`
opam install apron zarith menhir yojson
make

```

### RPM-based distributions

Tested on Fedora 27:

```shell
sudo dnf install git m4 redhat-rpm-config patch opam clang-devel-5.0.? llvm-devel-5.0.? gmp-devel mpfr-devel zlib-devel make which
opam init
opam switch 4.07.1
eval $(opam config env)
opam install apron zarith menhir yojson
make

```


## Usage

```shell
./bin/mopsa-c foo.c
./bin/mopsa-python -debug=python.flows._ foo.py
./bin/mopsa-c -unittest -debug=_unittest_summary analyzer/tests/c/int_tests.c
```

### Python Analysis

See `doc/python/type_analysis.md`.
