(lang dune 3.0)

(name okasaki-study)

(generate_opam_files true)

(package
 (name okasaki-study)
 (allow_empty)
 (depends
  ocaml
  dune
  (alcotest (>= 1.0))))
