# MOPSA

MOPSA stands for *Modular and Open Platform for Static Analysis*. It aims at easing the development and use of static analyzers.

More specifically, MOPSA is a generic framework for building sound static analyzer based on the theory of abstract interpretation.
MOPSA is independent of language and abstraction choices. 
Developers are free to add arbitrary abstractions (numeric, pointer, memory, etc.) and syntax iterators for new languages. 
Mopsa encourages the development of independent abstractions which can cooperate or be combined to improve precision. 

Mopsa currently support the analysis of Python, C and Python+C programs. Our [benchmarks](https://gitlab.com/mopsa/benchmarks/) provide an illustrative overview of what Mopsa can currently analyze. 
All analyses currently provided are flow and context-sensitive (i.e, control-flow operators are taken into account by the analysis, and functions are analyzed by virtual inlining).
The C analysis is actively developed and maintained. 
The Python and Python+C analyses work on real-world examples, but are not actively developed.

A [user manual](https://mopsa.gitlab.io/mopsa-analyzer/user-manual/) is available.

## License

Unless explicitly specified, the components of the MOPSA software are distributed under the terms of the GNU Lesser General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
See the accompanying COPYING file, or [http://www.gnu.org/licenses/](http://www.gnu.org/licenses/).

The documentation and example files of the MOPSA software are distributed under a Creative Commons Attribution-ShareAlike 4.0 International License. See [https://creativecommons.org/licenses/by-sa/4.0/](https://creativecommons.org/licenses/by-sa/4.0/).


## Installation 

You can use OCaml's package manager (opam), to resolve dependencies and install Mopsa. 
See [here on how to install opam](https://opam.ocaml.org/doc/Install.html#Using-your-distribution-39-s-package-system).

```shell
LANG=C opam pin add mopsa --with-doc --with-test .
```

You can check the [documentation](https://mopsa.gitlab.io/mopsa-analyzer/user-manual/quick-start/installation.html) to build from source.

### SV-Comp 

For the Software-Verification Competition, you also need:
```shell
 sudo dpkg --add-architecture i386 && sudo apt install libc6-dev-i386
```


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
* [user manual](https://mopsa.gitlab.io/mopsa-analyzer/user-manual/)
* [benchmark analysis projects](https://gitlab.com/mopsa/benchmarks) on GitLab
* [research project page](https://mopsa.lip6.fr/)
* [academic overview of Mopsa in STTE paper](https://hal.sorbonne-universite.fr/hal-02890500v1/document), and [in a PhD thesis](https://rmonat.fr/data/pubs/2021/thesis_monat.pdf#page=61)
