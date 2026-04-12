(lang dune 3.22)

(name sandworm)

(generate_opam_files true)

(source
 (github krfantasy/sandworm))

(authors "Jay Xu <jay.xu.krfantasy@gmail.com>")

(maintainers "Jay Xu <jay.xu.krfantasy@gmail.com>")

(license LICENSE)

(documentation https://url/to/documentation)

(package
 (name sandworm)
 (synopsis "Analog circuit modeling & SPICE netlist generator")
 (description "Analog circuit modeling")
 (depends
   ocaml
   menhir
   (alcotest :with-test))
 (tags
  ("circuit" "analog" "SPICE" "netlist" your project)))

(using menhir 3.0)

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
