(lang dune 3.7)

(name mopsa)

(generate_opam_files false)

(subst disabled)

(source
 (uri "https://gitlab.com/mopsa/mopsa-analyzer"))

(authors "The MOPSA team")

(maintainers "Antoine Miné <antoine.mine@lip6.fr>")

(license "LGPL-3.0-or-later")

(homepage "https://gitlab.com/mopsa/mopsa-analyzer")

(documentation "https://mopsa.gitlab.io/mopsa-manual/user-manual/")

(bug_reports "https://gitlab.com/mopsa/mopsa-analyzer/issues")

(using menhir 2.1)

(accept_alternative_dune_file_name)

(package
 (name mopsa)
 (synopsis "MOPSA: A Modular and Open Platform for Static Analysis using Abstract Interpretation")
 (description "MOPSA is a generic framework for building sound static analyzers based on Abstract Interpretation.
It features a modular architecture to support different kinds of languages, iterators, and abstract domains.
For the moment, MOPSA can analyze programs written in a subset of C and Python.
It reports run-time errors on C programs and uncaught exceptions on Python programs.")
 (depends (ocaml (>= 12.0)) dune conf-autoconf ocamlfind apron conf-libclang menhir mlgmpidl yojson zarith zlib)
 (depopts elina)
)

