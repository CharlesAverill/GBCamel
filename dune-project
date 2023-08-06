(lang dune 3.9)

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
 (depends 
  (ocaml (>= :"4.13.1")) 
  (core (>= :"0.15.1")) 
  dune
  (core_unix (>= :"0.15.2")) 
  (base64 (>= :"3.5.1"))
  (alcotest (and (>= :"1.7.0") (or :with-doc :with-test))) 
  (odoc (and (>= :"2.2.0") :with-doc))
  (ocamlformat (and (>= :"0.25.1") (or :with-doc :with-test)))
 )
)
