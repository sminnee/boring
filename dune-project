(lang dune 2.9)
(name boring)

(generate_opam_files true)

(authors "Sam Minnée")
(maintainers "sam.minnee@gmail.com")

(package
 (name boring)
 (synopsis "A boring language")
 (depends
  (mparser (>= 1.3))
  (ounit2 (>= 2.2.4))))