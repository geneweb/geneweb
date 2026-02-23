(lang dune 2.8)

(name geneweb)

; ignore formatting of generated dune file
(formatting
 (enabled_for ocaml))

(generate_opam_files true)
(license GPL-2.0-only)
(authors "Daniel de Rauglaudre")
(maintainers "Elie Canonici-Merle <elie.canonicimerle@geneanet.org>" "Olivier Pierre <olivier.pierre@geneanet.org>")
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
    calendars
    (camlp5 (>= "8.03.00"))
    camlp-streams
    (dune (>= "2.9.0"))
    (fmt :with-test)
    jingoo
    markup
    num
    (ocaml (>= 4.08))
    odoc
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
