(lang dune 3.7)

(name mopsa)

(generate_opam_files false)

(subst disabled)

(source
 (uri "https://gitlab.com/mopsa/mopsa-analyzer"))

(authors
  "Antoine Miné"
  "Abdelraouf Ouadjaout"
  "Matthieu Journault"
  "Aymeric Fromherz"
  "Raphaël Monat"
  "Francesco Parolini"
  "Marco Milanese"
  "Jérôme Boillot"
)

(maintainers
  "Antoine Miné <antoine.mine@lip6.fr>"
  "Abdelraouf Ouadjaout <ouadjaout@gmail.com>"
  "Raphaël Monat <raphael.monat@inria.fr>"
)

(license "LGPL-3.0-or-later")

(homepage "https://gitlab.com/mopsa/mopsa-analyzer")

(documentation "https://mopsa.gitlab.io/mopsa-analyzer/user-manual/")

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
 (depends (ocaml (>= 4.12.0)) dune conf-autoconf ocamlfind (apron (>= 0.9.15)) conf-libclang menhir mlgmpidl (yojson (>= 1.6.0)) (zarith (>= 1.4)) zlib)
 (depopts elina)
)
