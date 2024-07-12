# MOPSA

MOPSA stands for *Modular and Open Platform for Static Analysis*. It aims at making the development/use of static analyzers less painful.

More specifically, MOPSA is a generic framework for building sound static analyzer based on the theory of abstract interpretation.
It features a modular architecture for supporting different kinds of languages, iterators and abstractions.

For the moment, MOPSA can analyze only programs written in C and Python.

A [user manual](https://mopsa.gitlab.io/mopsa-analyzer/user-manual/) is available, as well as [example analysis projects](https://gitlab.com/mopsa/benchmarks).


## License

Unless explicitly specified, the components of the MOPSA software are distributed under the terms of the GNU Lesser General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
See the accompanying COPYING file, or [http://www.gnu.org/licenses/](http://www.gnu.org/licenses/).

The documentation and example files of the MOPSA software are distributed under a Creative Commons Attribution-ShareAlike 4.0 International License. See [https://creativecommons.org/licenses/by-sa/4.0/](https://creativecommons.org/licenses/by-sa/4.0/).


## Dependencies

Before compiling MOPSA, ensure that you have the following dependencies:

* Apron
* autoconf
* CamlIDL
* GMP
* LLVM + Clang (version >= 6.x)
* make
* Menhir
* MPFR
* OCaml (version >= 4.12.0)
* opam (version >= 2)
* Yojson
* Zarith

The OCaml dependencies can be installed on any system with opam with:

```shell
LANG=C opam install --deps-only --with-doc --with-test .
```

For instance, on Ubuntu, you can use these commands to install the dependencies (tested on Ubuntu 20.04):

```shell
sudo apt install opam llvm clang llvm-dev libclang-dev libclang-cpp10-dev libgmp-dev libmpfr-dev autoconf pkg-config
opam init --compiler 4.12.0
eval $(opam env)
LANG=C opam install --deps-only --with-doc --with-test .
```

For Ubuntu 22.04, you can use:
```shell
sudo apt install opam llvm clang llvm-dev libclang-dev libclang-cpp13-dev libgmp-dev libmpfr-dev pkg-config
opam init
eval $(opam env)
LANG=C opam install --deps-only --with-doc --with-test .
```

For SV-Comp, you also need:
```shell
 sudo dpkg --add-architecture i386 && sudo apt install libc6-dev-i386
```

## Compilation

To compile MOPSA, just run:

```shell
./configure
make
```

Note that the script `configure` accepts the usual configuration options (e.g. `--prefix`, `--bindir`, etc.), in addition to:

* `--disable-c` to disable the compilation of the C analysis.
* `--disable-python` to disable the compilation of the Python analysis.
* `--enable-byte` to compile MOPSA as a bytecode executable.

## Installation

After compilation, the binaries are available in the `_build/install/default/bin` sub-directory.

The standard command:
```shell
make install
```
will install MOPSA in your opam switch.

## Opam-based compilation & installation

Assuming that dependencies are installed, you can replace `./configure`, `make`, `make install` with simply:
```shell
opam pin add mopsa .
```
which compiles MOPSA and installs the binaries and libraries in your opam switch.


## Linking against the MOPSA library

MOPSA can also be used as a library to develop further tools.

It is installed as a `mopsa` ocamlfind package by `make install` or `opam`.
It contains several sub-packages, including various utilities (`mopsa.mopsa_utils`) and front-ends (`mopsa.mopsa_c_parser`, `mopsa.mopsa_c_stubs_parser`, `mopsa.mopsa_py_parser`, `mopsa.mopsa_universal_parser`, depending on which languages are enabled) and the toplevel `mopsa.mopsa_analyzer` package containing all the analysis logic and support for all compiled-in languages.

Consider the simple program `test.ml` that simulates the effect of the `mopsa` binary:
```ocaml
let _ = Mopsa_analyzer.Framework.Runner.run()
```
Add the following `dune` file in the same directory:
```dune
(executable
 (name test)
 (libraries mopsa.mopsa_analyzer))
```
Add also the file `dune-project`:
```dune
(lang dune 3.7)
```
Then, the project can be compiled with:
```shell
dune build
```

## Additional resources

* [source code](https://gitlab.com/mopsa/mopsa-analyzer) on GitLab
* [research project page](https://mopsa.lip6.fr/)
* [user manual](https://mopsa.gitlab.io/mopsa-analyzer/user-manual/)
* [benchmark analysis projects](https://gitlab.com/mopsa/benchmarks) on GitLab
