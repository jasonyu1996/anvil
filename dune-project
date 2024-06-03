(lang dune 3.8)

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
 (depends ocaml dune menhir)
 (tags
  (hdl safety fpga)))

(using menhir 2.1)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/dune-files.html#dune-project
