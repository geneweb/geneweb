(lang dune 2.9)

(name geneweb)

; ignore formatting of generated dune file
(formatting
 (enabled_for ocaml))

(generate_opam_files true)
(license GPL-2.0-only)
(authors "Daniel de Rauglaudre")
(maintainers "Elie Canonici-Merle <elie.canonicimerle@geneanet.org>")
(source (github geneweb/geneweb))
(homepage "https://geneweb.org")
(documentation "https://geneweb.tuxfamily.org/wiki/GeneWeb")

(package
  (name geneweb)
  (synopsis "Genealogy library and software")
  (description "GeneWeb is an open source genealogy software written in OCaml. It comes with a Web interface and can be used off-line or as a Web service.")
  (tags (genealogy))
  (depends
    (alcotest :with-test)
    ancient
    benchmark
    (calendars (= "1.0.0"))
    (camlp5 (>= "8.00.01"))
    camlp-streams
    cppo
    (fmt :with-test)
    jingoo
    markup
    num
    (ocaml (>= 4.08))
    (ocamlformat (and :dev (= 0.24.1)))
    ounit
    ppx_blob
    ppx_deriving
    ppx_import
    stdlib-shims
    syslog
    (unidecode (= "0.2.0"))
    uri
    uucp
    uunf
    uutf
    zarith
  ))
