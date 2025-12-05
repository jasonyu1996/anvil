(lang dune 3.13)

(name anvil)

(generate_opam_files true)

(source
 (github jasonyu1996/anvil))

(authors "Jason Yu")

(maintainers "Jason Yu")

(license LICENSE)

; (documentation https://url/to/documentation)

(package
 (name anvil)
 (synopsis "An HDL that helps avoid mistakes")
 (description "Anvil is a hardware description language with a type system designed to help the user avoid mistakes.")
 (depends
  (ocaml (>= 4.14.0))
  (dune (>= 3.8))
  menhir
  yojson
 )
 (tags
  (hdl safety fpga))
 (version 0.1.0))

(using menhir 3.0)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
