(lang dune 3.16)
(using menhir 3.0)

(name lambdaOcaml)
(license "MPL-2.0")

(generate_opam_files true)

(documentation https://url/to/documentation)

(package
 (name lambdaOcaml)
 (synopsis "Lambda calculus interpreter")
 (depends ocaml menhir))
