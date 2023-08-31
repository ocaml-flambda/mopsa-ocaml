(lang dune 2.9)

(name mopsa)

(generate_opam_files true)

(source
 (uri "https://gitlab.com/mopsa/mopsa-analyzer"))

(authors "The MOPSA team")

(maintainers "Antoine Miné <antoine.mine@lip6.fr>")

(license "GNU Lesser General Public License version 3")

(homepage "https://mopsa.lip6.fr/")

(documentation "https://mopsa.lip6.fr/mopsa/doc")

(bug_reports "https://gitlab.com/mopsa/mopsa-analyzer/issues")

(using menhir 2.1)

(package
 (name mopsa)
 (synopsis "MOPSA: A Modular and Open Platform for Static Analysis using Abstract Interpretation")
 (description "MOPSA is a generic framework for building sound static analyzers based on Abstract Interpretation.
It features a modular architecture to support different kinds of languages, iterators, and abstract domains.
For the moment, MOPSA can analyze programs written in a subset of C and Python.
It reports run-time errors on C programs and uncaught exceptions on Python programs.")
 (depends ocaml dune conf-autoconf ocamlfind apron conf-libclang menhir mlgmpidl yojson zarith sexplib ppx_sexp_conv))
