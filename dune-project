(lang dune 3.7)

(generate_opam_files true)

(name pogtranslator)
(source (github Deducteam/B-pog-translator))

(authors "Deducteam")
(maintainers "claude.stolze@inria.fr")
(license GNU GPL v3.0)

(package
 (name pogtranslator)
 (synopsis "Translate pog to TPTP or SMT-Lib using Why3")
 (description "TODO description")
 (depends
   (ocaml (>= 4.14.0))
   (markup (>= 1.03))
   (why3 (and (>= 1.6.0) (< 1.7~)))))