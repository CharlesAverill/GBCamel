(lang dune 3.7)

(name gbcamel)

(generate_opam_files true)

(source
 (github CharlesAverill/GBCamel))

(authors "Charles Averill")
(maintainers "Charles Averill")

(license LICENSE)

(documentation https://www.github.com/CharlesAverill/GBCamel)

(package
 (name gbcamel)
 (synopsis "A Gameboy Color emulator written in OCaml")
 (description "A Gameboy Color emulator written in OCaml")
 (depends ocaml core dune core_unix core_thread (alcotest :with-test)))
