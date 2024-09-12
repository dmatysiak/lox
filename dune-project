(lang dune 3.15)

(name lox)

(generate_opam_files true)

(source
 (github dmatysiak/lox))

(authors "Daniel A. Matysiak")

(package
 (name lox)
 (synopsis "An OCaml implementation of Robert Nystrom's `lox` language")
 (depends ocaml dune))
