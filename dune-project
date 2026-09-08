(lang dune 3.23)

(name full_biabduction)

(generate_opam_files true)

(source
 (github cla25ber/full_biabduction))

(authors "Claudio Bernardoni cla.bernardoni25@gmail.com")

(maintainers "Claudio Bernardoni cla.bernardoni25@gmail.com")

(license MIT)

(package
 (name full_biabduction)
 (synopsis "Reference implementation of full bi-abduction for symbolic heaps")
 (description "A prototype implementation of the full bi-abduction procedure for symbolic heaps.")
 (depends 
   ocaml
   (ounit2 :with-test))
 (tags
  ("Separation logic" "Bi-abduction")))

; See the complete stanza docs at https://dune.readthedocs.io/en/stable/reference/dune-project/index.html
