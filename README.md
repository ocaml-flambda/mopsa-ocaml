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

* autoconf
* make
* ocaml (version >= 4.08.0)
* opam (version >= 2)
* apron
* llvm + clang (version >= 5.x)
* zarith
* menhir
* yojson
* gmp
* mpfr
* camlidl
* zlib


## Compilation and installation


### DEB-based distributions (Debian, Ubuntu)

Tested on Ubuntu 16.04:

```shell
sudo apt install build-essential m4 opam clang-5.0 llvm-5.0-dev libclang-5.0-dev libgmp-dev libmpfr-dev zlib1g-dev
opam init
opam switch 4.08.0
eval $(opam config env)
opam install apron zarith menhir yojson
./configure
make
make tests
```


### RPM-based distributions

Tested on Fedora 27:

```shell
sudo dnf install git m4 redhat-rpm-config patch opam clang-devel-5.0.? llvm-devel-5.0.? gmp-devel mpfr-devel zlib-devel make which
opam init
opam switch 4.08.0
eval $(opam config env)
opam install apron zarith menhir yojson
./configure
make
make tests
```

### Installation

After compilation, the binaries are available in the `bin` sub-directory.

The standard command:
```
sudo make install

```
will install the binaries in the default prefix (`/usr/local`), which you can change with the `--prefix DIR` configure option.
The OCaml libraries are installed by `ocamlfind` in its default directory (see `ocamlfind printconf destdir`).


### Opam-based installation

Assuming that dependencies are installed, you can replace `./configure`, `make`, `sudo make install` with simply:

```shell
opam pin add mopsa .
```
which compiles MOPSA and install the binaries and libraries in your opam switch.



## Example usage

```shell
./bin/mopsa-c benchmarks/c/examples/swap.c
./bin/mopsa-c benchmarks/c/examples/euclid.c -debug=print
./bin/mopsa-python benchmarks/python/basic/operators.py
./bin/mopsa-c -unittest analyzer/tests/c/int_tests.c
```


## Linking against the MOPSA library

MOPSA can also be used as a library to develop further tools.

It is installed as a `mopsa` ocamlfind package by `make install` or `opam`.
It contains several sub-packages, including various utilities (`mopsa.utils`) and front-ends (`mopsa.clang_parser`, `mopsa.cstub_parser`, `mopsa.python_parser`, `mopsa.universal_parser`, depending on which languages are enabled) and the toplevel `mopsa.analyzer` package containing all the analysis logic and support for all compiled-in languages.

Consider the simple program `test.ml` that simulates the effect of the `mopsa` binary:
```ocaml
let _ = Framework.Runner.run()
```
It can be compiled with:
```shell
ocamlfind ocamlopt -thread -package mopsa.analyzer -linkpkg test.ml
```

## Additional resources

* [Full documentation](https://mopsa.lip6.fr/mopsa/doc/)
* [Mopsa project page](https://mopsa.lip6.fr/)
