(lang dune 3.16)
(using menhir 3.0)

(name cat_i)
(license "MPL-2.0")

(generate_opam_files true)

(documentation https://url/to/documentation)

(package
 (name cat_i)
 (synopsis "Lambda calculus interpreter")
 (depends ocaml menhir))
